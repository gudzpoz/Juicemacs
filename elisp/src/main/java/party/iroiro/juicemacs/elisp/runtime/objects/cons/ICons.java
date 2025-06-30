package party.iroiro.juicemacs.elisp.runtime.objects.cons;

import com.oracle.truffle.api.nodes.UnexpectedResultException;

public sealed interface ICons permits ArrayCons, SimpleCons, SubArrayCons {
    Object car();
    default long carLong() throws UnexpectedResultException {
        Object car = car();
        if (car instanceof Long l) {
            return l;
        }
        throw new UnexpectedResultException(car);
    }
    default double carDouble() throws UnexpectedResultException {
        Object car = car();
        if (car instanceof Double d) {
            return d;
        }
        throw new UnexpectedResultException(car);
    }
    void setCar(Object car);
    default void setCarLong(long carLong) {
        setCar(carLong);
    }
    default void setCarDouble(double carDouble) {
        setCar(carDouble);
    }

    Object cdr();
    void setCdr(Object cdr);
}
