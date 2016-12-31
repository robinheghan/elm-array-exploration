var _Skinney$elm_array_exploration$Native_JsArray = function() {

var empty = [];

function singleton(val) {
    return [val];
}

function length(arr) {
    return arr.length;
}

function initialize(size, offset, f) {

    // Optimize for common pattern of 32-sized arrays
    var result;
    if (size === 32) {
        result = array32();
    } else {
        result = new Array(size);
    }

    for (var i = 0; i < size; i++) {
        result[i] = f(offset + i);
    }

    return result;
}

function array32() {
    return [
        null,
        null,
        null,
        null,
        null,
        null,
        null,
        null,
        null,
        null,
        null,
        null,
        null,
        null,
        null,
        null,
        null,
        null,
        null,
        null,
        null,
        null,
        null,
        null,
        null,
        null,
        null,
        null,
        null,
        null,
        null,
        null
    ];
}

function listInitialize(ls, max) {
    var result;
    if (max === 32) {
        result = array32();
    } else {
        result = new Array(max);
    }

    var i = 0;

    for (; i < max; i++) {
        if (ls.ctor === '[]') {
            result.length = i;
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

    var result;
    if (len === 32) {
        result = array32();
    } else {
        result = new Array(len);
    }

    for (var i = 0; i < len; i++) {
        result[i] = f(arr[i]);
    }

    return result;
}

function indexedMap(f, offset, arr) {
    var len = arr.length;

    var result;
    if (len === 32) {
        result = array32();
    } else {
        result = new Array(len);
    }

    for (var i = 0; i < len; i++) {
        result[i] = A2(f, offset + i, arr[i]);
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

    var size = destLen + itemsToCopy;

    var result;
    if (size === 32) {
        result = array32();
    } else {
        result = new Array(size);
    }

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
    indexedMap: F3(indexedMap),
    slice: F3(slice),
    merge: F3(merge)
};

}();
