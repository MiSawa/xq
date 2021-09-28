def not: if . then false else true end;
def select(f): if f then . else empty end;
def map(f): [.[] | f];

def recurse(f; cond): def r: ., (f | select(cond) | r); r;
def recurse(f): recurse(f; . != null);
def recurse: recurse(.[]?);

def while(cond; update): def _while: if cond then ., (update | _while) else empty end; _while;
def repeat(exp): def _repeat: exp, _repeat; _repeat;

def range($from; $upto; $by): $from | while(($by > 0 and . < $upto) or ($by < 0 and . > $upto); . + $by);
def range($from; $upto): $from | while(. < $upto; . + 1);
def range($upto): range(0; $upto);

def isempty(f): reduce f as $_ (true; false);
def limit($n; f): foreach f as $item (0; . + 1; if . <= $n then $item else empty end);
def first(f): label $out | f | ., break $out;
def last(f): reduce f as $item (null; $item);
def nth($n; expr): last(limit($n + 1; expr));
def first: .[0];
def last: .[-1];
def nth($n): .[$n];