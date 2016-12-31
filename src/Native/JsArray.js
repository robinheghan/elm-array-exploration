var _Skinney$elm_array_exploration$Native_JsArray = function() {

var empty = [];

function singleton(val) {
    return [val];
}

function length(arr) {
    return arr.length;
}

function initialize(size, offset, f) {
    if (size === 32) {
        return initialize32(offset, f);
    }

    var result = [];

    for (var i = 0; i < size; i++) {
        result[i] = f(offset + i);
    }

    return result;
}

function initialize32(offset, f) {
    return [
        f(offset),
        f(offset + 1),
        f(offset + 2),
        f(offset + 3),
        f(offset + 4),
        f(offset + 5),
        f(offset + 6),
        f(offset + 7),
        f(offset + 8),
        f(offset + 9),
        f(offset + 10),
        f(offset + 11),
        f(offset + 12),
        f(offset + 13),
        f(offset + 14),
        f(offset + 15),
        f(offset + 16),
        f(offset + 17),
        f(offset + 18),
        f(offset + 19),
        f(offset + 20),
        f(offset + 21),
        f(offset + 22),
        f(offset + 23),
        f(offset + 24),
        f(offset + 25),
        f(offset + 26),
        f(offset + 27),
        f(offset + 28),
        f(offset + 29),
        f(offset + 30),
        f(offset + 31)
    ];
}

function listInitialize(ls, max) {
    var result = [];
    var i = 0;

    for (; i < max; i++) {
        if (ls.ctor === '[]') {
            break;
        }

        result[i] = ls._0;
        ls = ls._1;
    }

    return {
        ctor: '_Tuple2',
        _0: ls,
        _1: result
    };
}

function unsafeGet(idx, arr) {
    return arr[idx];
}

function unsafeSet(idx, val, arr) {
    var result = arr.slice();
    result[idx] = val;
    return result;
}

function push(val, arr) {
    var result = arr.slice();
    result.push(val);
    return result;
}

function foldl(f, init, arr) {
    var a = init;
    var len = arr.length;

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
    var len = arr.length;
    var result = new Array(len);

    for (var i = 0; i < len; i++) {
        result[i] = f(arr[i]);
    }

    return result;
}

function slice(from, to, arr) {
    return arr.slice(from, to);
}

function merge(dest, source, max) {
    var destLen = dest.length;
    var itemsToCopy = max - destLen;

    if (itemsToCopy > source.length) {
        itemsToCopy = source.length;
    }

    var result = new Array(destLen + itemsToCopy);

    for (var i = 0; i < destLen; i++) {
        result[i] = dest[i];
    }

    for (var i = 0; i < itemsToCopy; i++) {
        result[i + destLen] = source[i];
    }

    return result;
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
