var _Skinney$elm_array_exploration$Native_JsArray = function() {
/* A thin, but immutable, wrapper over native Javascript arrays. */

var empty = [];

function singleton(val) {
    return [val];
}

function length(arr) {
    return arr.length;
}

function initialize(size, offset, f) {
    var res = new Array(size);

    for (var i = 0; i < size; i++) {
        res[i] = f(offset + i);
    }

    return res;
}

// Create array from Elm list, containing at most max elements.
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

// No bounds checking, use with caution!
function unsafeGet(idx, arr) {
    return arr[idx];
}

// No bounds checking, use with caution!
function unsafeSet(idx, val, arr) {
    var res = arr.slice();
    res[idx] = val;
    return res;
}

function push(val, arr) {
    var res = copy(arr, 1);
    res[arr.length] = val;
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

// Appends dest onto source, and makes sure it has max elements.
function merge(dest, source, max) {
    var destLen = dest.length,
        toCopy = max - destLen,
        sourceStop = toCopy > source.length ? source.length : toCopy,
        arr = copy(dest, sourceStop);

    for (var i = 0; i < sourceStop; i++) {
        arr[i + destLen] = source[i];
    }

    return arr;
}

return {
    empty: empty,
    singleton: singleton,
    length: length,
    initialize: F3(initialize),
    listInitialize: F2(listInitialize),
    unsafeGet: F2(unsafeGet),
    unsafeSet: F3(unsafeSet),
    push: F2(push),
    foldl: F3(foldl),
    foldr: F3(foldr),
    map: F2(map),
    slice: F3(slice),
    merge: F3(merge)
};

}();
