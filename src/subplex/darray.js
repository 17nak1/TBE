let yHandeler = {
    get : function (target, prop, receiver) {
        if (typeof prop === 'symbol' || isNaN(+prop))
            return target[prop]
        return target.arr[(target.base - 1) + target.i - 1 + ((parseInt(prop) - 1)*(target.u1 + 1 - 1))]
    },
    set : function (target, prop, value, receiver) {
        target.arr[(target.base - 1) + target.i - 1 + ((parseInt(prop)  - 1)*(target.u1 + 1 - 1))] = value   
    }
}
let xHandeler = {
    get : function (target, prop, receiver) {       
            if (typeof prop === 'symbol' || isNaN(+prop))
                return target[prop]
            target.i = parseInt(prop)  
            if(target.i <= 0)
                target.i = 1              
            if(target.u1 === undefined && target.u2 === undefined)
                return target.arr[(target.base - 1) + target.i - 1]
            return new Proxy(target, yHandeler)
    },
    set : function (target, prop, value, receiver) {         
            if (typeof prop === 'symbol' || isNaN(+prop) || target.u1 !== undefined || target.u2 !== undefined)
                  return 0;                  
            target.i = parseInt(prop)
            if(target.i <= 0)
                target.i = 1  
            target.arr[(target.base - 1) + target.i - 1] = value; 
    }
}

let dArray  = function(base,u1,u2,arr){
    this.arr = arr === undefined ? [] : arr
    this.base = base === undefined ? 1 : base;  
    this.u1 = u1;
    this.u2 = u2;
    this.i = 1;
    return new Proxy(this, xHandeler)
}

dArray.prototype.clone = function(base,u1,u2) {
    if(base === undefined){
        base = 1
    }
    return new dArray(this.base + base - 1,u1,u2,this.arr);
};

module.exports = dArray;

Array.prototype.dArray = function(base,u1,u2) {
    return new dArray(base,u1,u2,this);
};