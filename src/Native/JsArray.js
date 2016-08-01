var _user$project$Native_JsArray = function() {
/* A thin, but still immutable, wrapper over native Javascript arrays. */

// An empty array
var empty = {
    ctor: 'JsArray',
    _0: []
};

function singleton(val) {
    return {
        ctor: 'JsArray',
        _0: [val]
    };
}

function initialize(size, f) {
    var res = new Array(size);

    for (var i = 0; i < size; i++) {
        res[i] = f(i);
    }

    return {
        ctor: 'JsArray',
        _0: res
    };
}

function length(arr) {
    return arr._0.length;
}

function get(idx, arr) {
    if (idx < 0 || idx >= length(arr)) {
        return _elm_lang$core$Maybe$Nothing;
    }

    return _elm_lang$core$Maybe$Just(arr._0[idx]);
}

function set(idx, val, arr) {
    if (idx < 0 || idx >= length(arr)) {
        return arr;
    }

    var copy = arr._0.slice();
    copy[idx] = val;

    return {
        ctor: 'JsArray',
        _0: copy
    };
}

function push(val, arr) {
    var copy = arr._0.slice();
    copy.push(val);

    return {
        ctor: 'JsArray',
        _0: copy
    };
}

function foldl(f, init, arr) {
    var a = init,
        len = length(arr);

    for (var i = 0; i < len; i++) {
        a = A2(f, arr._0[i], a);
    }

    return a;
}

function foldr(f, init, arr) {
    var a = init;

    for (var i = length(arr) - 1; i >= 0; i--) {
        a = A2(f, arr._0[i], a);
    }

    return a;
}

function slice(from, to, arr) {
    return {
        ctor: 'JsArray',
        _0: arr._0.slice(from, to)
    };
}

return {
    empty: empty,
    singleton: singleton,
    initialize: F2(initialize),
    length: length,
    get: F2(get),
    set: F3(set),
    push: F2(push),
    foldl: F3(foldl),
    foldr: F3(foldr),
    slice: F3(slice)
};

}();