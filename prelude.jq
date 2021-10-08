def isnormal: error("todo");
def isfinite: error("todo");

def not: if . then false else true end;
def select(f): if f then . else empty end;
def map(f): [.[] | f];
def map_values(f): .[] |= f;

def nulls: select(type == "null");
def booleans: select(type == "boolean");
def numbers: select(type == "number");
def strings: select(type == "string");
def arrays: select(type == "array");
def objects: select(type == "object");
def iterables: select(type | . == "array" or . == "object");
def normals: select(isnormal);
def finites: select(isfinite);
def values: select(type != "null");
def scalars: select(type != "array" and type != "object");

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

def keys_unsorted: [path(.[])[]];
def keys: keys_unsorted | sort;

def del(f): delpaths([path(f)]);
def setpath($paths; $v): getpath($paths) |= $v;

def to_entries: [keys[] as $key | {$key, value: .[$key]}];
def from_entries: reduce .[] as $entry ({}; .[$entry.key]=$entry.value);
def with_entries(f): to_entries | map(f) | from_entries;

def paths: path(..) | select(length > 0);
def paths(f): paths as $v | select(getpath($v) | f | $v);
def leaf_paths: paths(scalars);

def add: reduce .[] as $v (null; . + $v);
def any(g; f): reduce (g | f) as $v (false; . or $v);
def any(f): any(.[]; f);
def any: any(.);

def all(g; f): reduce (g | f) as $v (true; . and $v);
def all(f): all(.[]; f);
def all: all(.);

def sort_by(f): map([f, .]) | sort | map(.[1]);
