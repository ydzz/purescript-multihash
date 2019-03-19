"use strict";

var varint = require("varint");

exports.encode = function(x) {
    return varint.encode(x)
}

exports.decode_ = function(b) {
    return function(n){
        return [varint.decode(b,n),varint.decode.bytes]
    }
}