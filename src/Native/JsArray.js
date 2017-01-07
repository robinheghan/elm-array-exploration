var _Skinney$elm_array_exploration$Native_JsArray = function() {

var empty = [];

function singleton(val) {
    return [val];
}

function length(arr) {
    return arr.length;
}

function initialize(size, offset, f) {
    var result = newArray(size);

    for (var i = 0; i < size; i++) {
        result[i] = f(offset + i);
    }

    return result;
}

function newArray(size) {
    // A JS literal is much faster than `new Array(size)` in Safari.
    // The following code optimizes the common case of 32-sized arrays,
    // while falling back to the "proper" way to preallocate arrays
    // for other sizes. This makes a big performance difference in
    // Safari, while exerting a minor performance hit in Chrome.
    // For 32-sized arrays, Chrome and Safari become equally fast.
    if (size !== 32) {
        return new Array(size);
    }

    return [
        null, null, null, null, null,
        null, null, null, null, null,
        null, null, null, null, null,
        null, null, null, null, null,
        null, null, null, null, null,
        null, null, null, null, null,
        null, null
    ];
}

function initializeFromList(max, ls) {
    var result = newArray(max);

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
    var acc = init;
    var len = arr.length;

    for (var i = 0; i < len; i++) {
        acc = A2(f, arr[i], acc);
    }

    return acc;
}

function foldr(f, init, arr) {
    var acc = init;

    for (var i = arr.length - 1; i >= 0; i--) {
        acc = A2(f, arr[i], acc);
    }

    return acc;
}

function map(f, arr) {
    var len = arr.length;
    var result = newArray(len);

    for (var i = 0; i < len; i++) {
        result[i] = f(arr[i]);
    }

    return result;
}

function indexedMap(f, offset, arr) {
    var len = arr.length;
    var result = newArray(len);

    for (var i = 0; i < len; i++) {
        result[i] = A2(f, offset + i, arr[i]);
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
    var result = newArray(size);

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
