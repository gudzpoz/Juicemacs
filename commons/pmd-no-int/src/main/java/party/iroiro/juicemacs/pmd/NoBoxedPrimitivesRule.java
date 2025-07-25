package party.iroiro.juicemacs.pmd;

import net.sourceforge.pmd.lang.java.ast.*;
import net.sourceforge.pmd.lang.java.rule.AbstractJavaRule;
import net.sourceforge.pmd.lang.java.types.JArrayType;
import net.sourceforge.pmd.lang.java.types.JMethodSig;
import net.sourceforge.pmd.lang.java.types.JPrimitiveType;
import net.sourceforge.pmd.lang.java.types.JTypeMirror;
import net.sourceforge.pmd.lang.rule.RuleTargetSelector;
import net.sourceforge.pmd.properties.PropertyDescriptor;
import net.sourceforge.pmd.properties.PropertyFactory;
import org.checkerframework.checker.nullness.qual.NonNull;

import java.util.List;
import java.util.Objects;

@SuppressWarnings("unchecked")
public class NoBoxedPrimitivesRule extends AbstractJavaRule {
    private static final PropertyDescriptor<List<PrimitiveType>> PRIMITIVE_TYPES =
            PropertyFactory.enumListProperty("primitiveTypes", PrimitiveType.class, PrimitiveType::getName)
                    .defaultValue(List.of(
                            PrimitiveType.BYTE,
                            PrimitiveType.CHAR,
                            PrimitiveType.SHORT,
                            PrimitiveType.INT,
                            PrimitiveType.FLOAT
                    ))
                    .desc("List of primitive types to check boxing")
                    .build();

    public NoBoxedPrimitivesRule() {
        definePropertyDescriptor(PRIMITIVE_TYPES);
    }

    @Override
    protected @NonNull RuleTargetSelector buildTargetSelector() {
        return RuleTargetSelector.forTypes(
                ASTArgumentList.class,
                ASTArrayInitializer.class,
                ASTAssignmentExpression.class,

                ASTLocalVariableDeclaration.class,
                ASTReturnStatement.class,
                ASTLambdaExpression.class, // for implicit returns in lambda expressions

                ASTFieldDeclaration.class
        );
    }

    private boolean isSelectedPrimitives(JTypeMirror type) {
        for (PrimitiveType primitiveType : getProperty(PRIMITIVE_TYPES)) {
            if (type.isPrimitive(primitiveType.kind)) {
                return true;
            }
        }
        return false;
    }

    private void checkType(JTypeMirror coercedType, TypeNode node, Object data) {
        if (coercedType.isPrimitive()) {
            return;
        }

        JTypeMirror type = node.getTypeMirror();
        if (!type.isPrimitive()) {
            // Disallow `Integer -> Object`
            if (type.isBoxedPrimitive() && isSelectedPrimitives(type.unbox()) && !coercedType.equals(type.unbox())) {
                asCtx(data).addViolation(node, type, node.getOriginalText());
            }
            return;
        }

        if (!isSelectedPrimitives(type)) {
            return;
        }
        JPrimitiveType primitive = (JPrimitiveType) type;
        if (coercedType.isBoxedPrimitive()) {
            if (coercedType.unbox().isPrimitive(primitive.getKind())) {
                // Disallow `int -> Integer`
                asCtx(data).addViolation(node, primitive, node.getOriginalText());
            }
            // else: `int -> Long` simply does not compile
        } else {
            // Disallow `int -> Object`
            asCtx(data).addViolation(node, primitive, node.getOriginalText());
        }
    }

    @Override
    public Object visit(ASTArgumentList node, Object data) {
        // Disallow `methodCall((Object) 1)`
        JavaNode parent = node.getParent();
        JMethodSig method = switch (parent) {
            case ASTMethodCall call -> call.getMethodType();
            case ASTConstructorCall call -> call.getMethodType();
            case ASTExplicitConstructorInvocation invocation -> invocation.getMethodType();
            case ASTEnumConstant enumConstant -> enumConstant.getMethodType();
            default -> throw new UnsupportedOperationException("Unsupported parent type: " + parent);
        };
        boolean varargs;
        List<JTypeMirror> parameters;
        parameters = method.getFormalParameters();
        varargs = method.isVarargs();
        if (!parameters.isEmpty()) {
            int i;
            for (i = 0; i < parameters.size() - 1; i++) {
                checkType(parameters.get(i), node.get(i), data);
            }
            if (varargs) {
                JTypeMirror componentType = ((JArrayType) parameters.getLast()).getComponentType();
                for (; i < node.size(); i++) {
                    checkType(componentType, node.get(i), data);
                }
            } else {
                checkType(parameters.get(i), node.get(i), data);
            }
        }
        return super.visit(node, data);
    }

    @Override
    public Object visit(ASTArrayInitializer node, Object data) {
        // Disallow `Object[] arr = {1, 2, 3};`
        JavaNode parent = node.getParent();
        JTypeMirror arrayType = switch (parent) {
            case ASTArrayAllocation allocation ->
                    ((ASTArrayType) Objects.requireNonNull(allocation.getFirstChild())).getTypeMirror();
            case ASTVariableDeclarator declarator ->
                    Objects.requireNonNull(declarator.getParent().firstChild(ASTArrayType.class)).getTypeMirror();
            case ASTArrayInitializer initializer -> initializer.getTypeMirror();
            case null, default -> throw new UnsupportedOperationException("Unsupported parent type: " + parent);
        };
        JTypeMirror componentType = ((JArrayType) arrayType).getComponentType();
        for (ASTExpression expression : node) {
            checkType(componentType, expression, data);
        }
        return super.visit(node, data);
    }

    @Override
    public Object visit(ASTAssignmentExpression node, Object data) {
        // Disallow `a = (Object) 1;`
        ASTAssignableExpr target = node.getLeftOperand();
        ASTExpression value = node.getRightOperand();
        checkType(target.getTypeMirror(), value, data);
        return super.visit(node, data);
    }

    private void visitDeclarators(JTypeMirror variableType, JavaNode declaratorParent, Object data) {
        // Disallow `Object a = 1;`
        for (int i = 2; i < declaratorParent.getNumChildren(); i++) {
            ASTVariableDeclarator declarator = (ASTVariableDeclarator) declaratorParent.getChild(i);
            ASTExpression initializer = declarator.getInitializer();
            if (initializer != null) {
                checkType(variableType, initializer, data);
            }
        }
    }

    @Override
    public Object visit(ASTLocalVariableDeclaration node, Object data) {
        ASTType typeNode = node.getTypeNode();
        // In case of `var a` declaration
        if (typeNode != null) {
            visitDeclarators(typeNode.getTypeMirror(), node, data);
        }
        return super.visit(node, data);
    }

    @Override
    public Object visit(ASTReturnStatement node, Object data) {
        ASTExpression expr = node.getExpr();
        if (expr != null) {
            JavaNode parent = node.getParent();
            JTypeMirror returnType;
            while (true) {
                if (parent instanceof ASTMethodDeclaration declaration) {
                    returnType = declaration.getResultTypeNode().getTypeMirror();
                    break;
                } else if (parent instanceof ASTLambdaExpression lambda) {
                    returnType = lambda.getFunctionalMethod().getReturnType();
                    break;
                } else {
                    parent = parent.getParent();
                }
            }
            checkType(returnType, expr, data);
        }
        return super.visit(node, data);
    }

    @Override
    public Object visit(ASTLambdaExpression node, Object data) {
        JTypeMirror returnType = node.getFunctionalMethod().getReturnType();
        if (node.isExpressionBody()) {
            checkType(returnType, node.getExpressionBody(), data);
        }
        return super.visit(node, data);
    }

    @Override
    public Object visit(ASTFieldDeclaration node, Object data) {
        visitDeclarators(node.getTypeNode().getTypeMirror(), node, data);
        return super.visit(node, data);
    }

    public enum PrimitiveType {
        BYTE("byte", JPrimitiveType.PrimitiveTypeKind.BYTE),
        CHAR("char", JPrimitiveType.PrimitiveTypeKind.CHAR),
        SHORT("short", JPrimitiveType.PrimitiveTypeKind.SHORT),
        INT("int", JPrimitiveType.PrimitiveTypeKind.INT),
        LONG("long", JPrimitiveType.PrimitiveTypeKind.LONG),
        FLOAT("float", JPrimitiveType.PrimitiveTypeKind.FLOAT),
        DOUBLE("double", JPrimitiveType.PrimitiveTypeKind.DOUBLE);

        private final String name;
        private final JPrimitiveType.PrimitiveTypeKind kind;

        PrimitiveType(String name, JPrimitiveType.PrimitiveTypeKind kind) {
            this.name = name;
            this.kind = kind;
        }

        public String getName() {
            return name;
        }

        public JPrimitiveType.PrimitiveTypeKind getKind() {
            return kind;
        }
    }
}
