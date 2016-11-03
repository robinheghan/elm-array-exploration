// import Maybe

var _Skinney$elm_array_exploration$Native_JsArray = function() {
/* A thin, but still immutable, wrapper over native Javascript arrays. */

// An empty array
var empty = [];

function singleton(val) {
    return [val];
}

function initialize(size, offset, f) {
    var res = new Array(size);

    for (var i = 0; i < size; i++) {
        res[i] = f(offset + i);
    }

    return res;
}

function listInitialize(ls, max) {
    var res = new Array(max);
    var i = 0;

    for (; i < max; i++) {
        if (ls.ctor === '[]') {
            break;
        }

        res[i] = ls._0;
        ls = ls._1;
    }

    res.length = i;

    return {
        ctor: '_Tuple2',
        _0: ls,
        _1: res
    };
}

function length(arr) {
    return arr.length;
}

function get(idx, arr) {
    if (idx < 0 || idx >= arr.length) {
        return _elm_lang$core$Maybe$Nothing;
    }

    return _elm_lang$core$Maybe$Just(arr[idx]);
}

function unsafeGet(idx, arr) {
    return arr[idx];
}

function set(idx, val, arr) {
    if (idx < 0 || idx >= arr.length) {
        return arr;
    }

    var res = copy(arr, 0);
    res[idx] = val;
    return res;
}

function copy(arr, offset) {
    var len = arr.length,
        res = new Array(len + offset);

    for (var i = 0; i < len; i++) {
        res[i] = arr[i];
    }

    return res;
}

function push(val, arr) {
    var res = copy(arr, 1);
    res[arr.length] = val;
    return res;
}

function foldl(f, init, arr) {
    var a = init,
        len = arr.length;

    for (var i = 0; i < len; i++) {
        a = A2(f, arr[i], a);
    }

    return a;
}

function foldr(f, init, arr) {
    var a = init;

    for (var i = arr.length - 1; i >= 0; i--) {
        a = A2(f, arr[i], a);
    }

    return a;
}

function map(f, arr) {
    var len = arr.length,
        copy = new Array(len);

    for (var i = 0; i < len; i++) {
        copy[i] = f(arr[i]);
    }

    return copy;
}

function slice(from, to, arr) {
    return arr.slice(from, to);
}

function merge(dest, source, max) {
    var destLen = dest.length,
        toCopy = max - destLen,
        sourceStop = toCopy > source.length ? source.length : toCopy,
        len = destLen + sourceStop,
        arr = new Array(len);

    for (var i = 0; i < destLen; i++) {
        arr[i] = dest[i];
    }

    for (var i = 0; i < sourceStop; i++) {
        arr[i + destLen] = source[i];
    }

    return arr;
}

return {
    empty: empty,
    singleton: singleton,
    initialize: F3(initialize),
    listInitialize: F2(listInitialize),
    length: length,
    get: F2(get),
    unsafeGet: F2(unsafeGet),
    set: F3(set),
    push: F2(push),
    foldl: F3(foldl),
    foldr: F3(foldr),
    map: F2(map),
    slice: F3(slice),
    merge: F3(merge)
};

}();