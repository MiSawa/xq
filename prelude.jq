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

def sort_by(f): [keys[] as $i | [[.[$i] | f], $i, .[$i]]] | sort | map(.[2]);
def min_by(f): reduce (.[] | [[f], .]) as [$cmp, $val] (null; if . == null or .[0] >  $cmp then [$cmp, $val] end) | .[1];
def max_by(f): reduce (.[] | [[f], .]) as [$cmp, $val] (null; if . == null or .[0] <= $cmp then [$cmp, $val] end) | .[1];
def min: min_by(.);
def max: max_by(.);
def unique_by(f): def __yes_i_know_this_is_bad:.; [group_by(f)[] | .[0]];
def unique: unique_by(.);

def flatten: if type == "array" then [.[] | flatten] | reduce .[] as $v ([]; . + $v) else [.] end;
def flatten($d): if ($d >= 0) and (type == "array") then [.[] | flatten($d-1)] | reduce .[] as $v ([]; . + $v) else [.] end;

def index($s): def __yes_i_know_this_is_bad:.; indices($s)[0];
def rindex($s): def __yes_i_know_this_is_bad:.; indices($s)[-1];

def combinations: if length == 0 then [] else (.[0].[] as $car | [$car] + (.[1:] | combinations)) end;
def combinations($n): [limit($n; repeat(.))] | combinations;

def ltrimstr($s): if startswith($s) then .[($s | length):] else . end;
def rtrimstr($s): if endswith($s) then .[:-($s | length)] else . end;

def join($sep): if length == 0 then "" else reduce .[] as $e (null; (if . == null then "" else . + $sep end) + ($e | if . == null then "" elif type | . == "boolean" or . == "number" or . == "string" then tostring else error("Unsupported element \(.) on join") end)) end;
def ascii_downcase: explode | map(if . >= 65 and . <= 90 then .+32 end) | implode;
def ascii_upcase: explode | map(if . >= 97 and . <= 122 then .-32 end) | implode;

def transpose: . as $dot | [range(max_by(length) | length) as $i | $dot | map(.[$i])];

def todateiso8601: strftime("%Y-%m-%dT%H:%M:%SZ");
def fromdate: fromdateiso8601;
def todate: todateiso8601;

def match($r; $f): __split_match_impl($r; $f) | .[] | objects;
def match($r): if $r | type == "array" then match($r[0]; $r[1]) else match($r; null) end;
def test($r; $f): isempty(match($r; $f)) | not;
def test($r): if $r | type == "array" then test($r[0]; $r[1]) else test($r; null) end;
def capture($r; $f): match($r; $f) | [.captures[] | select(.name) | {key: .name, value: .string}] | from_entries;
def capture($r): if $r | type == "array" then capture($r[0]; $r[1]) else capture($r; null) end;
def scan($r; $f): match($r; $f + "g") | if .captures | length == 0 then .string else .captures | [.[].string] end;
def scan($r): if $r | type == "array" then scan($r[0]; $r[1]) else scan($r; null) end;
def splits($r; $f): __split_match_impl($r; $f + "g") | .[] | strings;
def splits($r): splits($r; null);
def split($r; $f): [splits($r; $f)];
def sub($r; s; $f):
    def combinations2: if length == 0 then [] else (.[-1].[] as $last | (.[:-1] | combinations2) + [$last]) end;
    __split_match_impl($r; $f) | map(if type == "string" then [.] else [.captures[] | select(.name) | {key: .name, value: .string}] | from_entries | [s] end) | combinations2 | add;
def sub($r; s): sub($r; s; null);
def gsub($r; s; $f): sub($r; s; $f + "g");
def gsub($r; s): gsub($r; s; null);

def inputs: try repeat(input) catch if . == "NoMoreInputError" then empty else error end;

