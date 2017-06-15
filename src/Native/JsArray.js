var _Skinney$elm_array_exploration$Native_JsArray = function() {

var empty = [];

function singleton(val) {
    return [val];
}

function length(arr) {
    return arr.length;
}

function initialize(size, offset, f) {
    var result = new Array(size);

    while (size--) {
        result[size] = f(offset + size);
    }

    return result;
}

function initializeFromList(max, ls) {
    var result = new Array(max);

    for (var i = 0; i < max; i++) {
        if (ls.ctor === '[]') {
            result.length = i;
            break;
        }

        result[i] = ls._0;
        ls = ls._1;
    }

    return {
        ctor: '_Tuple2',
        _0: result,
        _1: ls
    };
}

function unsafeGet(idx, arr) {
    return arr[idx];
}

function unsafeSet(idx, val, arr) {
    var length = arr.length;
    var result = new Array(length);

    while (length--) {
        result[length] = arr[length];
    }

    result[idx] = val;
    return result;
}

function push(val, arr) {
    var length = arr.length;
    var result = new Array(length + 1);
    result[length] = val;

    while (length--) {
        result[length] = arr[length];
    }

    return result;
}

function foldl(f, acc, arr) {
    var len = arr.length;

    for (var i = 0; i < len; i++) {
        acc = A2(f, arr[i], acc);
    }

    return acc;
}

function foldr(f, acc, arr) {
    var length = arr.length;

    while (length--) {
        acc = A2(f, arr[length], acc);
    }

    return acc;
}

function map(f, arr) {
    var length = arr.length;
    var result = new Array(length);

    while (length--) {
        result[length] = f(arr[length]);
    }

    return result;
}

function indexedMap(f, offset, arr) {
    var length = arr.length;
    var result = new Array(length);

    while (length--) {
        result[length] = A2(f, offset + length, arr[length]);
    }

    return result;
}

function slice(from, to, arr) {
    return arr.slice(from, to);
}

function appendN(n, dest, source) {
    var destLen = dest.length;
    var itemsToCopy = n - destLen;

    if (itemsToCopy > source.length) {
        itemsToCopy = source.length;
    }

    var size = destLen + itemsToCopy;
    var result = new Array(size);

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
    initializeFromList: F2(initializeFromList),
    unsafeGet: F2(unsafeGet),
    unsafeSet: F3(unsafeSet),
    push: F2(push),
    foldl: F3(foldl),
    foldr: F3(foldr),
    map: F2(map),
    indexedMap: F3(indexedMap),
    slice: F3(slice),
    appendN: F3(appendN)
};

}();
