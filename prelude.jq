def sub($a; $b): def __unimplemented:.; "sub(\(.), \($a), \($b))";

def not: if . then false else true end;
def select(f): if f then . else empty end;
def map(f): [.[] | f];
def map_values(f): .[] |= f;

def isfinite: isinfinite | not;
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
def walk(f): def _walk: if type | . == "array" or . == "object" then map_values(_walk) | f else f end; _walk;
def walk2(f): if type | . == "array" or . == "object" then map_values(walk2(f)) | f else f end;

def while(cond; update): def _while: if cond then ., (update | _while) else empty end; _while;
def until(cond; update): def _until: if cond then . else (update | _until) end; _until;
def repeat(exp): def _repeat: exp, _repeat; _repeat;

def range($from; $upto; $by): $from | while(($by > 0 and . < $upto) or ($by < 0 and . > $upto); . + $by);
def range($from; $upto): $from | while(. < $upto; . + 1);
def range($upto): range(0; $upto);

def isempty(f): label $outer | (f | false, break $outer), true;
def limit($n; f): label $outer | foreach f as $item (0; . + 1; if . <= $n then $item else break $outer end);
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
def paths(f): paths as $v | select(getpath($v) | f) | $v;
def leaf_paths: paths(scalars);

def add: reduce .[] as $v (null; . + $v);
def any(g; f): isempty(g | select(f)) | not;
def any(f): any(.[]; f);
def any: any(.);

def all(g; f): isempty(g | select(f | not));
def all(f): all(.[]; f);
def all: all(.);

def sort_by(f): map([f, .]) | sort | map(.[1]);
def min_by(f): def __yes_i_know_this_is_bad:.; sort_by(f)[0];
def max_by(f): def __yes_i_know_this_is_bad:.; sort_by(f)[-1];
def min: min_by(.);
def max: max_by(.);
def unique_by(f): def __yes_i_know_this_is_bad:.; [group_by(f)[] | .[0]];
def unique: unique_by(.);

def flatten: if type | . == "array" or . == "object" then [.[] | flatten] | reduce .[] as $v ([]; . + $v) else [.] end;
def flatten($d): if ($d >= 0) and (type | . == "array" or . == "object") then [.[] | flatten($d-1)] | add else [.] end;

def index($s): def __yes_i_know_this_is_bad:.; indices($s)[0];
def rindex($s): def __yes_i_know_this_is_bad:.; indices($s)[-1];

def combinations: if length == 0 then [] else (.[0].[] as $car | [$car] + (.[1:] | combinations)) end;
def combinations($n): [limit($n; repeat(.))] | combinations;

def ltrimstr($s): if startswith($s) then .[($s | length):] else . end;
def rtrimstr($s): if endswith($s) then .[:-($s | length)] else . end;

def join($sep): def stringify: label $out | ((nulls | ("", break $out)), ((booleans, numbers, strings) | (tostring, break $out)), error("Unsupported element \(.) on join")); if length == 0 then "" else reduce (.[1:][] | stringify) as $e (.[0] | stringify; . + $sep + $e) end;
def ascii_downcase: explode | map(if . >= 65 and . <= 90 then .+32 end) | implode;
def ascii_upcase: explode | map(if . >= 97 and . <= 122 then .-32 end) | implode;

def transpose: . as $dot | [range(max_by(length) | length) as $i | $dot | map(.[$i])];

def fromdateiso8601: strptime("%+") | mktime;
def todateiso8601: strftime("%Y-%m-%dT%H:%M:%SZ");
def fromdate: fromdateiso8601;
def todate: todateiso8601;

