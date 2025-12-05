/*---------------------------------------------------------------------------------------------
 * Copyright (c) 2024 gudzpoz
 * Copyright (c) 2015 - present Microsoft Corporation
 *
 * Permission is hereby granted, free of charge, to any person obtaining a copy
 * of this software and associated documentation files (the "Software"), to deal
 * in the Software without restriction, including without limitation the rights
 * to use, copy, modify, merge, publish, distribute, sublicense, and/or sell
 * copies of the Software, and to permit persons to whom the Software is
 * furnished to do so, subject to the following conditions:
 *
 * The above copyright notice and this permission notice shall be included in all
 * copies or substantial portions of the Software.
 *
 * THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR
 * IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY,
 * FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT. IN NO EVENT SHALL THE
 * AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER
 * LIABILITY, WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING FROM,
 * OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS IN THE
 * SOFTWARE.
 *
 * See LICENSE.piece-tree for license information.
 *--------------------------------------------------------------------------------------------*/

package party.iroiro.juicemacs.piecetree.interval;

import org.jspecify.annotations.Nullable;
import org.junit.jupiter.api.Test;
import org.opentest4j.AssertionFailedError;
import party.iroiro.juicemacs.piecetree.interval.IntervalTreeTest.Operation.*;

import java.util.ArrayList;
import java.util.Objects;
import java.util.Random;

import static org.junit.jupiter.api.Assertions.assertEquals;

public class IntervalTreeTest {
    static final class Range {
        long start;
        long end;
        boolean frontAdvance;
        boolean rearAdvance;

        Range(long start, long end) {
            this(start, end, false, false);
        }
        Range(long start, long end, boolean frontAdvance, boolean rearAdvance) {
            this.start = start;
            this.end = end;
            this.frontAdvance = frontAdvance;
            this.rearAdvance = rearAdvance;
        }

        @Override
        public boolean equals(Object o) {
            if (!(o instanceof Range range)) return false;
            return start == range.start && end == range.end;
        }

        @Override
        public int hashCode() {
            return Objects.hash(start, end);
        }

        @Override
        public String toString() {
            return "Range{" +
                    "start=" + start +
                    ", end=" + end +
                    '}';
        }

        public int compare(Range range) {
            if (start == range.start) {
                return Long.compare(end, range.end);
            }
            return Long.compare(start, range.start);
        }
    }
    static final class Oracle {
        private final ArrayList<Range> intervals = new ArrayList<>();

        Range insert(Range interval) {
            intervals.add(interval);
            intervals.sort(Range::compare);
            return interval;
        }

        void delete(Range interval) {
            for (int i = 0; i < intervals.size(); i++) {
                if (intervals.get(i) == interval) {
                    intervals.remove(i);
                    break;
                }
            }
        }

        ArrayList<Range> search(Range interval) {
            ArrayList<Range> result = new ArrayList<>();
            for (Range range : intervals) {
                if (range.start <= interval.end && range.end >= interval.start) {
                    result.add(range);
                }
            }
            return result;
        }

        void acceptReplace(long offset, long deleted, long inserted) {
            for (Range range : intervals) {
                if (range.end < offset) {
                    continue;
                }
                if (range.start > offset + deleted) {
                    long delta = inserted - deleted;
                    range.start += delta;
                    range.end += delta;
                    continue;
                }
                // the logic is quite complex: instead of replicating it, we leave that to tests
                Interval interval = new Interval(range.start, range.end);
                interval.setFrontAdvance(range.frontAdvance);
                interval.setRearAdvance(range.rearAdvance);
                interval.acceptEdit(offset, offset + deleted, inserted);
                range.start = interval.start;
                range.end = interval.end;
            }
        }
    }
    sealed interface Operation {
        record Insert(long begin, long end, boolean frontAdvance, boolean rearAdvance) implements Operation {
            Insert(long begin, long end) {
                this(begin, end, false, false);
            }
        }
        record Delete(int id) implements Operation {}
        record Change(int id, long begin, long end) implements Operation {}
        record Search(long begin, long end, boolean assertExact) implements Operation {
            Search(long begin, long end) {
                this(begin, end, false);
            }
        }
        record Replace(long begin, long end, long textLength) implements Operation {}
    }
    static final class TestState {
        private final Oracle oracle = new Oracle();
        private final IntervalTree tree = new IntervalTree();
        private final ArrayList<@Nullable Interval> treeNodes = new ArrayList<>();
        private final ArrayList<@Nullable Range> oracleNodes = new ArrayList<>();
        private int timestamp = 0;

        void acceptOperation(Operation op) {
            timestamp++;
            switch (op) {
                case Insert(long begin, long end, boolean front, boolean rear) -> {
                    Interval node = new Interval(begin, end);
                    node.setFrontAdvance(front);
                    node.setRearAdvance(rear);
                    treeNodes.add(node);
                    tree.insert(node);
                    oracleNodes.add(oracle.insert(new Range(begin, end, front, rear)));
                    assertEquals(oracleNodes.size(), treeNodes.size());
                }
                case Delete(int id) -> {
                    tree.delete(Objects.requireNonNull(treeNodes.get(id)));
                    oracle.delete(Objects.requireNonNull(oracleNodes.get(id)));
                    treeNodes.set(id, null);
                    oracleNodes.set(id, null);
                }
                case Change(int id, long begin, long end) -> {
                    Interval interval = Objects.requireNonNull(treeNodes.get(id));
                    tree.delete(interval);
                    interval.reset(0, begin, end);
                    tree.insert(interval);

                    Range range = Objects.requireNonNull(oracleNodes.get(id));
                    oracle.delete(range);
                    range.start = begin;
                    range.end = end;
                    oracle.insert(range);
                }
                case Search(long begin, long end, boolean assertExact) -> {
                    ArrayList<Range> nodesInRange = new ArrayList<>();
                    tree.intervalSearch(begin, end, timestamp,
                            (i) -> nodesInRange.add(new Range(i.cachedAbsoluteStart, i.cachedAbsoluteEnd)));
                    ArrayList<Range> expected = oracle.search(new Range(begin, end));
                    nodesInRange.sort(Range::compare);
                    expected.sort(Range::compare);
                    assertEquals(expected, nodesInRange);
                    if (assertExact) {
                        assertEquals(1, nodesInRange.size());
                        assertEquals(new Range(begin, end), expected.get(0));
                    }
                }
                case Replace(long begin, long end, long textLength) -> {
                    long length = end - begin;
                    tree.acceptReplace(begin, length, textLength);
                    oracle.acceptReplace(begin, length, textLength);
                }
            }
        }

        int getExistingNodeId(int index) {
            int currIndex = -1;
            for (int i = 0; i < treeNodes.size(); i++) {
                if (treeNodes.get(i) == null) {
                    continue;
                }
                currIndex++;
                if (currIndex == index) {
                    return i;
                }
            }
            throw new IllegalArgumentException("Invalid index");
        }

        static IntervalTree testIntervalTree(Operation... operations) {
            TestState state = new TestState();
            for (Operation op : operations) {
                state.acceptOperation(op);
            }
            state.acceptOperation(new Search(0, Long.MAX_VALUE));
            return state.tree;
        }
    }

    @Test
    public void testIntervalTree() {
        TestState.testIntervalTree(
                new Insert(28, 35),
                new Search(30, 31),
                new Insert(52, 54),
                new Search(30, 31),
                new Search(53, 54),
                new Insert(63, 69),
                new Search(30, 31),
                new Search(53, 54),
                new Search(64, 65)
        );
    }

    @Test
    public void testSimpleEdits() {
        // insert @ to the left
        TestState.testIntervalTree(
                new Insert(5, 10),
                new Replace(0, 0, 5),
                new Search(10, 15, true)
        );
        // insert @ to the right
        TestState.testIntervalTree(
                new Insert(5, 10),
                new Replace(15, 15, 5),
                new Search(5, 10, true)
        );
        // insert @ middle
        TestState.testIntervalTree(
                new Insert(5, 10),
                new Replace(7, 7, 5),
                new Search(5, 15, true)
        );
        // delete @ to the left
        TestState.testIntervalTree(
                new Insert(5, 10),
                new Replace(0, 5, 0),
                new Search(0, 5, true)
        );
        // delete @ to the right
        TestState.testIntervalTree(
                new Insert(5, 10),
                new Replace(10, 15, 0),
                new Search(5, 10, true)
        );
        // delete @ middle
        TestState.testIntervalTree(
                new Insert(5, 10),
                new Replace(6, 9, 0),
                new Search(5, 7, true)
        );
    }

    @Test
    public void testEdgeEdits() {
        record TestCase(Replace op, Search case1, Search case2) {}
        record ComparativeTest(
                Insert interval1,
                Insert interval2,
                TestCase[] cases
        ) {
            void run() {
                for (TestCase test : cases) {
                    TestState.testIntervalTree(interval1, test.op, test.case1);
                    TestState.testIntervalTree(interval2, test.op, test.case2);
                }
            }
        }

        new ComparativeTest(
                // front advance
                new Insert(4, 8, true, false),
                // no front advance
                new Insert(4, 8, false, false),
                new TestCase[]{
                        new TestCase(
                                new Replace(4, 4, 4),
                                // front advance
                                new Search(8, 12, true),
                                // no front advance
                                new Search(4, 12, true)
                        ),
                        new TestCase(
                                new Replace(2, 4, 0), // delete front
                                // same
                                new Search(2, 6, true),
                                new Search(2, 6, true)
                        ),
                        new TestCase(
                                new Replace(2, 4, 4), // delete + insert front
                                // different
                                new Search(6, 10, true),
                                new Search(4, 10, true)
                        ),
                        new TestCase(
                                new Replace(2, 6, 4), // delete front&inner + insert
                                // same
                                new Search(4, 8, true),
                                new Search(4, 8, true)
                        ),
                }
        ).run();

        new ComparativeTest(
                // rear advance
                new Insert(4, 8, false, true),
                // no rear advance
                new Insert(4, 8, false, false),
                new TestCase[]{
                        new TestCase(
                                new Replace(8, 8, 4),
                                // rear advance
                                new Search(4, 12, true),
                                // no rear advance
                                new Search(4, 8, true)
                        ),
                        new TestCase(
                                new Replace(8, 12, 0), // delete rear
                                // same
                                new Search(4, 8, true),
                                new Search(4, 8, true)
                        ),
                        new TestCase(
                                new Replace(6, 8, 0), // delete rear
                                // same
                                new Search(4, 6, true),
                                new Search(4, 6, true)
                        ),
                        new TestCase(
                                new Replace(8, 12, 4), // delete + insert rear
                                // same
                                new Search(4, 8, true),
                                new Search(4, 8, true)
                        ),
                        new TestCase(
                                new Replace(6, 8, 4), // delete + insert rear
                                // different
                                new Search(4, 10, true),
                                new Search(4, 8, true)
                        ),
                        new TestCase(
                                new Replace(6, 10, 1), // delete front&inner + insert
                                // same
                                new Search(4, 7, true),
                                new Search(4, 7, true)
                        ),
                }
        ).run();
    }

    static final class AutoTest {
        public static final int MIN_INSERTS = 1;
        public static final int MIN_CHANGE_CNT = 10;

        private final ArrayList<Operation> history = new ArrayList<>();
        private final TestState state = new TestState();
        private final Random random;
        private int insertCnt;
        private int deleteCnt;
        private int changeCnt;
        private final int maxIntervalEnd;
        private final boolean applyEdits;

        AutoTest(int seed, int maxIntervalEnd, int maxInserts, int maxChanges, boolean applyEdits) {
            this.maxIntervalEnd = maxIntervalEnd;
            random = new Random(seed);
            insertCnt = random.nextInt(MIN_INSERTS, maxInserts);
            changeCnt = random.nextInt(MIN_CHANGE_CNT, maxChanges);
            this.applyEdits = applyEdits;
            deleteCnt = 0;
        }

        private void run(Operation operation) {
            history.add(operation);
            state.acceptOperation(operation);
        }

        public void run() {
            try {
                while (insertCnt > 0 || deleteCnt > 0 || changeCnt > 0) {
                    if (insertCnt > 0) {
                        doRandomInsert();
                        insertCnt--;
                        deleteCnt++;
                    } else if (changeCnt > 0) {
                        doRandomChange();
                        changeCnt--;
                    } else {
                        doRandomDelete();
                        deleteCnt--;
                    }

                    int begin = random.nextInt(maxIntervalEnd);
                    int end = random.nextInt(begin, maxIntervalEnd);
                    run(new Search(begin, end));
                    run(new Search(0, Long.MAX_VALUE));
                }
            } catch (AssertionFailedError error) {
                throw new AssertionFailedError(printHistory(), error.getExpected(), error.getActual(), error.getCause());
            }
        }

        private void doRandomDelete() {
            int idx = random.nextInt(deleteCnt);
            int id = state.getExistingNodeId(idx);
            run(new Delete(id));
        }

        private void doRandomChange() {
            if (applyEdits && random.nextBoolean()) {
                int offset = random.nextInt(maxIntervalEnd);
                int deletes, inserts;
                if (random.nextInt(10) <= 2) {
                    // large range
                    deletes = random.nextInt(maxIntervalEnd / 2);
                    inserts = random.nextInt(maxIntervalEnd / 2);
                } else {
                    deletes = random.nextInt(10);
                    inserts = random.nextInt(10);
                }
                run(new Replace(offset, offset + deletes, inserts));
            } else {
                int idx = random.nextInt(deleteCnt);
                int id = state.getExistingNodeId(idx);
                int begin = random.nextInt(maxIntervalEnd);
                int end = random.nextInt(begin, maxIntervalEnd);
                run(new Change(id, begin, end));
            }
        }

        private void doRandomInsert() {
            int begin = random.nextInt(maxIntervalEnd);
            int end = random.nextInt(begin, maxIntervalEnd);
            run(new Insert(begin, end));
        }

        public String printHistory() {
            StringBuilder sb = new StringBuilder();
            for (Operation operation : history) {
                sb.append(operation).append("\n");
            }
            return sb.toString();
        }
    }

    @Test
    public void testAutoNoEdit() {
        for (int i = 0; i < 10000; i++) {
            AutoTest test = new AutoTest(i, 100, 30, 30, false);
            test.run();
        }
        for (int i = 0; i < 1000; i++) {
            AutoTest test = new AutoTest(i, 10000, 300, 300, false);
            test.run();
        }
    }

    @Test
    public void testAutoEdit() {
        for (int i = 0; i < 10000; i++) {
            AutoTest test = new AutoTest(i, 100, 30, 30, true);
            test.run();
        }
        for (int i = 0; i < 1000; i++) {
            AutoTest test = new AutoTest(i, 10000, 300, 300, true);
            test.run();
        }
    }

    @Test
    public void testStrangeInterval() {
        IntervalTree tree = TestState.testIntervalTree(
                new Insert(4, 4, true, false),
                // front should not advance over rear
                new Replace(4, 4, 4)
        );
        ArrayList<Interval> intervals = new ArrayList<>(1);
        tree.intervalSearch(0, Long.MAX_VALUE, 0, intervals::add);
        assertEquals(1, intervals.size());
        Interval actual = intervals.get(0);
        assertEquals(new Range(4, 4), new Range(actual.start, actual.end));
    }

    @Test
    public void testGetAll() {
        IntervalTree tree = TestState.testIntervalTree(
                new Insert(0, 0),
                new Insert(4, 4)
        );
        ArrayList<Interval> intervals = new ArrayList<>(1);
        tree.intervalSearch(0, 4, 0, intervals::add);
        assertEquals(2, intervals.size());
        Interval actual = intervals.get(0);
        assertEquals(new Range(0, 0), new Range(actual.start, actual.end));
        actual = intervals.get(1);
        assertEquals(new Range(4, 4), new Range(actual.start, actual.end));
    }
}
