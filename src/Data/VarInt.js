"use strict";

var varint = require("varint");

exports.encode = function(x) {
    return varint.encode(x)
}

exports.decode = function(b) {
    return function(n){
        return varint.decode(b,n)
    }
}