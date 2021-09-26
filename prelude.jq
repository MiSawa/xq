def not: if . then false else true end;
def select(f): if f then . else empty end;
def recurse(f; cond): def r: ., (f | select(cond) | r); r;
def recurse(f): recurse(f; . != null);
def recurse: recurse(.[]?);