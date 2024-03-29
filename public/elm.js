
(function() {
'use strict';

function F2(fun)
{
  function wrapper(a) { return function(b) { return fun(a,b); }; }
  wrapper.arity = 2;
  wrapper.func = fun;
  return wrapper;
}

function F3(fun)
{
  function wrapper(a) {
    return function(b) { return function(c) { return fun(a, b, c); }; };
  }
  wrapper.arity = 3;
  wrapper.func = fun;
  return wrapper;
}

function F4(fun)
{
  function wrapper(a) { return function(b) { return function(c) {
    return function(d) { return fun(a, b, c, d); }; }; };
  }
  wrapper.arity = 4;
  wrapper.func = fun;
  return wrapper;
}

function F5(fun)
{
  function wrapper(a) { return function(b) { return function(c) {
    return function(d) { return function(e) { return fun(a, b, c, d, e); }; }; }; };
  }
  wrapper.arity = 5;
  wrapper.func = fun;
  return wrapper;
}

function F6(fun)
{
  function wrapper(a) { return function(b) { return function(c) {
    return function(d) { return function(e) { return function(f) {
    return fun(a, b, c, d, e, f); }; }; }; }; };
  }
  wrapper.arity = 6;
  wrapper.func = fun;
  return wrapper;
}

function F7(fun)
{
  function wrapper(a) { return function(b) { return function(c) {
    return function(d) { return function(e) { return function(f) {
    return function(g) { return fun(a, b, c, d, e, f, g); }; }; }; }; }; };
  }
  wrapper.arity = 7;
  wrapper.func = fun;
  return wrapper;
}

function F8(fun)
{
  function wrapper(a) { return function(b) { return function(c) {
    return function(d) { return function(e) { return function(f) {
    return function(g) { return function(h) {
    return fun(a, b, c, d, e, f, g, h); }; }; }; }; }; }; };
  }
  wrapper.arity = 8;
  wrapper.func = fun;
  return wrapper;
}

function F9(fun)
{
  function wrapper(a) { return function(b) { return function(c) {
    return function(d) { return function(e) { return function(f) {
    return function(g) { return function(h) { return function(i) {
    return fun(a, b, c, d, e, f, g, h, i); }; }; }; }; }; }; }; };
  }
  wrapper.arity = 9;
  wrapper.func = fun;
  return wrapper;
}

function A2(fun, a, b)
{
  return fun.arity === 2
    ? fun.func(a, b)
    : fun(a)(b);
}
function A3(fun, a, b, c)
{
  return fun.arity === 3
    ? fun.func(a, b, c)
    : fun(a)(b)(c);
}
function A4(fun, a, b, c, d)
{
  return fun.arity === 4
    ? fun.func(a, b, c, d)
    : fun(a)(b)(c)(d);
}
function A5(fun, a, b, c, d, e)
{
  return fun.arity === 5
    ? fun.func(a, b, c, d, e)
    : fun(a)(b)(c)(d)(e);
}
function A6(fun, a, b, c, d, e, f)
{
  return fun.arity === 6
    ? fun.func(a, b, c, d, e, f)
    : fun(a)(b)(c)(d)(e)(f);
}
function A7(fun, a, b, c, d, e, f, g)
{
  return fun.arity === 7
    ? fun.func(a, b, c, d, e, f, g)
    : fun(a)(b)(c)(d)(e)(f)(g);
}
function A8(fun, a, b, c, d, e, f, g, h)
{
  return fun.arity === 8
    ? fun.func(a, b, c, d, e, f, g, h)
    : fun(a)(b)(c)(d)(e)(f)(g)(h);
}
function A9(fun, a, b, c, d, e, f, g, h, i)
{
  return fun.arity === 9
    ? fun.func(a, b, c, d, e, f, g, h, i)
    : fun(a)(b)(c)(d)(e)(f)(g)(h)(i);
}

var _elm_lang$lazy$Native_Lazy = function() {

function memoize(thunk)
{
    var value;
    var isForced = false;
    return function(tuple0) {
        if (!isForced) {
            value = thunk(tuple0);
            isForced = true;
        }
        return value;
    };
}

return {
    memoize: memoize
};

}();

//import Native.Utils //

var _elm_lang$core$Native_Basics = function() {

function div(a, b)
{
	return (a / b) | 0;
}
function rem(a, b)
{
	return a % b;
}
function mod(a, b)
{
	if (b === 0)
	{
		throw new Error('Cannot perform mod 0. Division by zero error.');
	}
	var r = a % b;
	var m = a === 0 ? 0 : (b > 0 ? (a >= 0 ? r : r + b) : -mod(-a, -b));

	return m === b ? 0 : m;
}
function logBase(base, n)
{
	return Math.log(n) / Math.log(base);
}
function negate(n)
{
	return -n;
}
function abs(n)
{
	return n < 0 ? -n : n;
}

function min(a, b)
{
	return _elm_lang$core$Native_Utils.cmp(a, b) < 0 ? a : b;
}
function max(a, b)
{
	return _elm_lang$core$Native_Utils.cmp(a, b) > 0 ? a : b;
}
function clamp(lo, hi, n)
{
	return _elm_lang$core$Native_Utils.cmp(n, lo) < 0
		? lo
		: _elm_lang$core$Native_Utils.cmp(n, hi) > 0
			? hi
			: n;
}

var ord = ['LT', 'EQ', 'GT'];

function compare(x, y)
{
	return { ctor: ord[_elm_lang$core$Native_Utils.cmp(x, y) + 1] };
}

function xor(a, b)
{
	return a !== b;
}
function not(b)
{
	return !b;
}
function isInfinite(n)
{
	return n === Infinity || n === -Infinity;
}

function truncate(n)
{
	return n | 0;
}

function degrees(d)
{
	return d * Math.PI / 180;
}
function turns(t)
{
	return 2 * Math.PI * t;
}
function fromPolar(point)
{
	var r = point._0;
	var t = point._1;
	return _elm_lang$core$Native_Utils.Tuple2(r * Math.cos(t), r * Math.sin(t));
}
function toPolar(point)
{
	var x = point._0;
	var y = point._1;
	return _elm_lang$core$Native_Utils.Tuple2(Math.sqrt(x * x + y * y), Math.atan2(y, x));
}

return {
	div: F2(div),
	rem: F2(rem),
	mod: F2(mod),

	pi: Math.PI,
	e: Math.E,
	cos: Math.cos,
	sin: Math.sin,
	tan: Math.tan,
	acos: Math.acos,
	asin: Math.asin,
	atan: Math.atan,
	atan2: F2(Math.atan2),

	degrees: degrees,
	turns: turns,
	fromPolar: fromPolar,
	toPolar: toPolar,

	sqrt: Math.sqrt,
	logBase: F2(logBase),
	negate: negate,
	abs: abs,
	min: F2(min),
	max: F2(max),
	clamp: F3(clamp),
	compare: F2(compare),

	xor: F2(xor),
	not: not,

	truncate: truncate,
	ceiling: Math.ceil,
	floor: Math.floor,
	round: Math.round,
	toFloat: function(x) { return x; },
	isNaN: isNaN,
	isInfinite: isInfinite
};

}();
//import //

var _elm_lang$core$Native_Utils = function() {

// COMPARISONS

function eq(x, y)
{
	var stack = [];
	var isEqual = eqHelp(x, y, 0, stack);
	var pair;
	while (isEqual && (pair = stack.pop()))
	{
		isEqual = eqHelp(pair.x, pair.y, 0, stack);
	}
	return isEqual;
}


function eqHelp(x, y, depth, stack)
{
	if (depth > 100)
	{
		stack.push({ x: x, y: y });
		return true;
	}

	if (x === y)
	{
		return true;
	}

	if (typeof x !== 'object')
	{
		if (typeof x === 'function')
		{
			throw new Error(
				'Trying to use `(==)` on functions. There is no way to know if functions are "the same" in the Elm sense.'
				+ ' Read more about this at http://package.elm-lang.org/packages/elm-lang/core/latest/Basics#=='
				+ ' which describes why it is this way and what the better version will look like.'
			);
		}
		return false;
	}

	if (x === null || y === null)
	{
		return false
	}

	if (x instanceof Date)
	{
		return x.getTime() === y.getTime();
	}

	if (!('ctor' in x))
	{
		for (var key in x)
		{
			if (!eqHelp(x[key], y[key], depth + 1, stack))
			{
				return false;
			}
		}
		return true;
	}

	// convert Dicts and Sets to lists
	if (x.ctor === 'RBNode_elm_builtin' || x.ctor === 'RBEmpty_elm_builtin')
	{
		x = _elm_lang$core$Dict$toList(x);
		y = _elm_lang$core$Dict$toList(y);
	}
	if (x.ctor === 'Set_elm_builtin')
	{
		x = _elm_lang$core$Set$toList(x);
		y = _elm_lang$core$Set$toList(y);
	}

	// check if lists are equal without recursion
	if (x.ctor === '::')
	{
		var a = x;
		var b = y;
		while (a.ctor === '::' && b.ctor === '::')
		{
			if (!eqHelp(a._0, b._0, depth + 1, stack))
			{
				return false;
			}
			a = a._1;
			b = b._1;
		}
		return a.ctor === b.ctor;
	}

	// check if Arrays are equal
	if (x.ctor === '_Array')
	{
		var xs = _elm_lang$core$Native_Array.toJSArray(x);
		var ys = _elm_lang$core$Native_Array.toJSArray(y);
		if (xs.length !== ys.length)
		{
			return false;
		}
		for (var i = 0; i < xs.length; i++)
		{
			if (!eqHelp(xs[i], ys[i], depth + 1, stack))
			{
				return false;
			}
		}
		return true;
	}

	if (!eqHelp(x.ctor, y.ctor, depth + 1, stack))
	{
		return false;
	}

	for (var key in x)
	{
		if (!eqHelp(x[key], y[key], depth + 1, stack))
		{
			return false;
		}
	}
	return true;
}

// Code in Generate/JavaScript.hs, Basics.js, and List.js depends on
// the particular integer values assigned to LT, EQ, and GT.

var LT = -1, EQ = 0, GT = 1;

function cmp(x, y)
{
	if (typeof x !== 'object')
	{
		return x === y ? EQ : x < y ? LT : GT;
	}

	if (x instanceof String)
	{
		var a = x.valueOf();
		var b = y.valueOf();
		return a === b ? EQ : a < b ? LT : GT;
	}

	if (x.ctor === '::' || x.ctor === '[]')
	{
		while (x.ctor === '::' && y.ctor === '::')
		{
			var ord = cmp(x._0, y._0);
			if (ord !== EQ)
			{
				return ord;
			}
			x = x._1;
			y = y._1;
		}
		return x.ctor === y.ctor ? EQ : x.ctor === '[]' ? LT : GT;
	}

	if (x.ctor.slice(0, 6) === '_Tuple')
	{
		var ord;
		var n = x.ctor.slice(6) - 0;
		var err = 'cannot compare tuples with more than 6 elements.';
		if (n === 0) return EQ;
		if (n >= 1) { ord = cmp(x._0, y._0); if (ord !== EQ) return ord;
		if (n >= 2) { ord = cmp(x._1, y._1); if (ord !== EQ) return ord;
		if (n >= 3) { ord = cmp(x._2, y._2); if (ord !== EQ) return ord;
		if (n >= 4) { ord = cmp(x._3, y._3); if (ord !== EQ) return ord;
		if (n >= 5) { ord = cmp(x._4, y._4); if (ord !== EQ) return ord;
		if (n >= 6) { ord = cmp(x._5, y._5); if (ord !== EQ) return ord;
		if (n >= 7) throw new Error('Comparison error: ' + err); } } } } } }
		return EQ;
	}

	throw new Error(
		'Comparison error: comparison is only defined on ints, '
		+ 'floats, times, chars, strings, lists of comparable values, '
		+ 'and tuples of comparable values.'
	);
}


// COMMON VALUES

var Tuple0 = {
	ctor: '_Tuple0'
};

function Tuple2(x, y)
{
	return {
		ctor: '_Tuple2',
		_0: x,
		_1: y
	};
}

function chr(c)
{
	return new String(c);
}


// GUID

var count = 0;
function guid(_)
{
	return count++;
}


// RECORDS

function update(oldRecord, updatedFields)
{
	var newRecord = {};

	for (var key in oldRecord)
	{
		newRecord[key] = oldRecord[key];
	}

	for (var key in updatedFields)
	{
		newRecord[key] = updatedFields[key];
	}

	return newRecord;
}


//// LIST STUFF ////

var Nil = { ctor: '[]' };

function Cons(hd, tl)
{
	return {
		ctor: '::',
		_0: hd,
		_1: tl
	};
}

function append(xs, ys)
{
	// append Strings
	if (typeof xs === 'string')
	{
		return xs + ys;
	}

	// append Lists
	if (xs.ctor === '[]')
	{
		return ys;
	}
	var root = Cons(xs._0, Nil);
	var curr = root;
	xs = xs._1;
	while (xs.ctor !== '[]')
	{
		curr._1 = Cons(xs._0, Nil);
		xs = xs._1;
		curr = curr._1;
	}
	curr._1 = ys;
	return root;
}


// CRASHES

function crash(moduleName, region)
{
	return function(message) {
		throw new Error(
			'Ran into a `Debug.crash` in module `' + moduleName + '` ' + regionToString(region) + '\n'
			+ 'The message provided by the code author is:\n\n    '
			+ message
		);
	};
}

function crashCase(moduleName, region, value)
{
	return function(message) {
		throw new Error(
			'Ran into a `Debug.crash` in module `' + moduleName + '`\n\n'
			+ 'This was caused by the `case` expression ' + regionToString(region) + '.\n'
			+ 'One of the branches ended with a crash and the following value got through:\n\n    ' + toString(value) + '\n\n'
			+ 'The message provided by the code author is:\n\n    '
			+ message
		);
	};
}

function regionToString(region)
{
	if (region.start.line == region.end.line)
	{
		return 'on line ' + region.start.line;
	}
	return 'between lines ' + region.start.line + ' and ' + region.end.line;
}


// TO STRING

function toString(v)
{
	var type = typeof v;
	if (type === 'function')
	{
		return '<function>';
	}

	if (type === 'boolean')
	{
		return v ? 'True' : 'False';
	}

	if (type === 'number')
	{
		return v + '';
	}

	if (v instanceof String)
	{
		return '\'' + addSlashes(v, true) + '\'';
	}

	if (type === 'string')
	{
		return '"' + addSlashes(v, false) + '"';
	}

	if (v === null)
	{
		return 'null';
	}

	if (type === 'object' && 'ctor' in v)
	{
		var ctorStarter = v.ctor.substring(0, 5);

		if (ctorStarter === '_Tupl')
		{
			var output = [];
			for (var k in v)
			{
				if (k === 'ctor') continue;
				output.push(toString(v[k]));
			}
			return '(' + output.join(',') + ')';
		}

		if (ctorStarter === '_Task')
		{
			return '<task>'
		}

		if (v.ctor === '_Array')
		{
			var list = _elm_lang$core$Array$toList(v);
			return 'Array.fromList ' + toString(list);
		}

		if (v.ctor === '<decoder>')
		{
			return '<decoder>';
		}

		if (v.ctor === '_Process')
		{
			return '<process:' + v.id + '>';
		}

		if (v.ctor === '::')
		{
			var output = '[' + toString(v._0);
			v = v._1;
			while (v.ctor === '::')
			{
				output += ',' + toString(v._0);
				v = v._1;
			}
			return output + ']';
		}

		if (v.ctor === '[]')
		{
			return '[]';
		}

		if (v.ctor === 'Set_elm_builtin')
		{
			return 'Set.fromList ' + toString(_elm_lang$core$Set$toList(v));
		}

		if (v.ctor === 'RBNode_elm_builtin' || v.ctor === 'RBEmpty_elm_builtin')
		{
			return 'Dict.fromList ' + toString(_elm_lang$core$Dict$toList(v));
		}

		var output = '';
		for (var i in v)
		{
			if (i === 'ctor') continue;
			var str = toString(v[i]);
			var c0 = str[0];
			var parenless = c0 === '{' || c0 === '(' || c0 === '<' || c0 === '"' || str.indexOf(' ') < 0;
			output += ' ' + (parenless ? str : '(' + str + ')');
		}
		return v.ctor + output;
	}

	if (type === 'object')
	{
		if (v instanceof Date)
		{
			return '<' + v.toString() + '>';
		}

		if (v.elm_web_socket)
		{
			return '<websocket>';
		}

		var output = [];
		for (var k in v)
		{
			output.push(k + ' = ' + toString(v[k]));
		}
		if (output.length === 0)
		{
			return '{}';
		}
		return '{ ' + output.join(', ') + ' }';
	}

	return '<internal structure>';
}

function addSlashes(str, isChar)
{
	var s = str.replace(/\\/g, '\\\\')
			  .replace(/\n/g, '\\n')
			  .replace(/\t/g, '\\t')
			  .replace(/\r/g, '\\r')
			  .replace(/\v/g, '\\v')
			  .replace(/\0/g, '\\0');
	if (isChar)
	{
		return s.replace(/\'/g, '\\\'');
	}
	else
	{
		return s.replace(/\"/g, '\\"');
	}
}


return {
	eq: eq,
	cmp: cmp,
	Tuple0: Tuple0,
	Tuple2: Tuple2,
	chr: chr,
	update: update,
	guid: guid,

	append: F2(append),

	crash: crash,
	crashCase: crashCase,

	toString: toString
};

}();
var _elm_lang$core$Basics$never = function (_p0) {
	never:
	while (true) {
		var _p1 = _p0;
		var _v1 = _p1._0;
		_p0 = _v1;
		continue never;
	}
};
var _elm_lang$core$Basics$uncurry = F2(
	function (f, _p2) {
		var _p3 = _p2;
		return A2(f, _p3._0, _p3._1);
	});
var _elm_lang$core$Basics$curry = F3(
	function (f, a, b) {
		return f(
			{ctor: '_Tuple2', _0: a, _1: b});
	});
var _elm_lang$core$Basics$flip = F3(
	function (f, b, a) {
		return A2(f, a, b);
	});
var _elm_lang$core$Basics$always = F2(
	function (a, _p4) {
		return a;
	});
var _elm_lang$core$Basics$identity = function (x) {
	return x;
};
var _elm_lang$core$Basics_ops = _elm_lang$core$Basics_ops || {};
_elm_lang$core$Basics_ops['<|'] = F2(
	function (f, x) {
		return f(x);
	});
var _elm_lang$core$Basics_ops = _elm_lang$core$Basics_ops || {};
_elm_lang$core$Basics_ops['|>'] = F2(
	function (x, f) {
		return f(x);
	});
var _elm_lang$core$Basics_ops = _elm_lang$core$Basics_ops || {};
_elm_lang$core$Basics_ops['>>'] = F3(
	function (f, g, x) {
		return g(
			f(x));
	});
var _elm_lang$core$Basics_ops = _elm_lang$core$Basics_ops || {};
_elm_lang$core$Basics_ops['<<'] = F3(
	function (g, f, x) {
		return g(
			f(x));
	});
var _elm_lang$core$Basics_ops = _elm_lang$core$Basics_ops || {};
_elm_lang$core$Basics_ops['++'] = _elm_lang$core$Native_Utils.append;
var _elm_lang$core$Basics$toString = _elm_lang$core$Native_Utils.toString;
var _elm_lang$core$Basics$isInfinite = _elm_lang$core$Native_Basics.isInfinite;
var _elm_lang$core$Basics$isNaN = _elm_lang$core$Native_Basics.isNaN;
var _elm_lang$core$Basics$toFloat = _elm_lang$core$Native_Basics.toFloat;
var _elm_lang$core$Basics$ceiling = _elm_lang$core$Native_Basics.ceiling;
var _elm_lang$core$Basics$floor = _elm_lang$core$Native_Basics.floor;
var _elm_lang$core$Basics$truncate = _elm_lang$core$Native_Basics.truncate;
var _elm_lang$core$Basics$round = _elm_lang$core$Native_Basics.round;
var _elm_lang$core$Basics$not = _elm_lang$core$Native_Basics.not;
var _elm_lang$core$Basics$xor = _elm_lang$core$Native_Basics.xor;
var _elm_lang$core$Basics_ops = _elm_lang$core$Basics_ops || {};
_elm_lang$core$Basics_ops['||'] = _elm_lang$core$Native_Basics.or;
var _elm_lang$core$Basics_ops = _elm_lang$core$Basics_ops || {};
_elm_lang$core$Basics_ops['&&'] = _elm_lang$core$Native_Basics.and;
var _elm_lang$core$Basics$max = _elm_lang$core$Native_Basics.max;
var _elm_lang$core$Basics$min = _elm_lang$core$Native_Basics.min;
var _elm_lang$core$Basics$compare = _elm_lang$core$Native_Basics.compare;
var _elm_lang$core$Basics_ops = _elm_lang$core$Basics_ops || {};
_elm_lang$core$Basics_ops['>='] = _elm_lang$core$Native_Basics.ge;
var _elm_lang$core$Basics_ops = _elm_lang$core$Basics_ops || {};
_elm_lang$core$Basics_ops['<='] = _elm_lang$core$Native_Basics.le;
var _elm_lang$core$Basics_ops = _elm_lang$core$Basics_ops || {};
_elm_lang$core$Basics_ops['>'] = _elm_lang$core$Native_Basics.gt;
var _elm_lang$core$Basics_ops = _elm_lang$core$Basics_ops || {};
_elm_lang$core$Basics_ops['<'] = _elm_lang$core$Native_Basics.lt;
var _elm_lang$core$Basics_ops = _elm_lang$core$Basics_ops || {};
_elm_lang$core$Basics_ops['/='] = _elm_lang$core$Native_Basics.neq;
var _elm_lang$core$Basics_ops = _elm_lang$core$Basics_ops || {};
_elm_lang$core$Basics_ops['=='] = _elm_lang$core$Native_Basics.eq;
var _elm_lang$core$Basics$e = _elm_lang$core$Native_Basics.e;
var _elm_lang$core$Basics$pi = _elm_lang$core$Native_Basics.pi;
var _elm_lang$core$Basics$clamp = _elm_lang$core$Native_Basics.clamp;
var _elm_lang$core$Basics$logBase = _elm_lang$core$Native_Basics.logBase;
var _elm_lang$core$Basics$abs = _elm_lang$core$Native_Basics.abs;
var _elm_lang$core$Basics$negate = _elm_lang$core$Native_Basics.negate;
var _elm_lang$core$Basics$sqrt = _elm_lang$core$Native_Basics.sqrt;
var _elm_lang$core$Basics$atan2 = _elm_lang$core$Native_Basics.atan2;
var _elm_lang$core$Basics$atan = _elm_lang$core$Native_Basics.atan;
var _elm_lang$core$Basics$asin = _elm_lang$core$Native_Basics.asin;
var _elm_lang$core$Basics$acos = _elm_lang$core$Native_Basics.acos;
var _elm_lang$core$Basics$tan = _elm_lang$core$Native_Basics.tan;
var _elm_lang$core$Basics$sin = _elm_lang$core$Native_Basics.sin;
var _elm_lang$core$Basics$cos = _elm_lang$core$Native_Basics.cos;
var _elm_lang$core$Basics_ops = _elm_lang$core$Basics_ops || {};
_elm_lang$core$Basics_ops['^'] = _elm_lang$core$Native_Basics.exp;
var _elm_lang$core$Basics_ops = _elm_lang$core$Basics_ops || {};
_elm_lang$core$Basics_ops['%'] = _elm_lang$core$Native_Basics.mod;
var _elm_lang$core$Basics$rem = _elm_lang$core$Native_Basics.rem;
var _elm_lang$core$Basics_ops = _elm_lang$core$Basics_ops || {};
_elm_lang$core$Basics_ops['//'] = _elm_lang$core$Native_Basics.div;
var _elm_lang$core$Basics_ops = _elm_lang$core$Basics_ops || {};
_elm_lang$core$Basics_ops['/'] = _elm_lang$core$Native_Basics.floatDiv;
var _elm_lang$core$Basics_ops = _elm_lang$core$Basics_ops || {};
_elm_lang$core$Basics_ops['*'] = _elm_lang$core$Native_Basics.mul;
var _elm_lang$core$Basics_ops = _elm_lang$core$Basics_ops || {};
_elm_lang$core$Basics_ops['-'] = _elm_lang$core$Native_Basics.sub;
var _elm_lang$core$Basics_ops = _elm_lang$core$Basics_ops || {};
_elm_lang$core$Basics_ops['+'] = _elm_lang$core$Native_Basics.add;
var _elm_lang$core$Basics$toPolar = _elm_lang$core$Native_Basics.toPolar;
var _elm_lang$core$Basics$fromPolar = _elm_lang$core$Native_Basics.fromPolar;
var _elm_lang$core$Basics$turns = _elm_lang$core$Native_Basics.turns;
var _elm_lang$core$Basics$degrees = _elm_lang$core$Native_Basics.degrees;
var _elm_lang$core$Basics$radians = function (t) {
	return t;
};
var _elm_lang$core$Basics$GT = {ctor: 'GT'};
var _elm_lang$core$Basics$EQ = {ctor: 'EQ'};
var _elm_lang$core$Basics$LT = {ctor: 'LT'};
var _elm_lang$core$Basics$JustOneMore = function (a) {
	return {ctor: 'JustOneMore', _0: a};
};

//import Native.Utils //

var _elm_lang$core$Native_Debug = function() {

function log(tag, value)
{
	var msg = tag + ': ' + _elm_lang$core$Native_Utils.toString(value);
	var process = process || {};
	if (process.stdout)
	{
		process.stdout.write(msg);
	}
	else
	{
		console.log(msg);
	}
	return value;
}

function crash(message)
{
	throw new Error(message);
}

return {
	crash: crash,
	log: F2(log)
};

}();
var _elm_lang$core$Debug$crash = _elm_lang$core$Native_Debug.crash;
var _elm_lang$core$Debug$log = _elm_lang$core$Native_Debug.log;

var _elm_lang$core$Maybe$withDefault = F2(
	function ($default, maybe) {
		var _p0 = maybe;
		if (_p0.ctor === 'Just') {
			return _p0._0;
		} else {
			return $default;
		}
	});
var _elm_lang$core$Maybe$Nothing = {ctor: 'Nothing'};
var _elm_lang$core$Maybe$andThen = F2(
	function (callback, maybeValue) {
		var _p1 = maybeValue;
		if (_p1.ctor === 'Just') {
			return callback(_p1._0);
		} else {
			return _elm_lang$core$Maybe$Nothing;
		}
	});
var _elm_lang$core$Maybe$Just = function (a) {
	return {ctor: 'Just', _0: a};
};
var _elm_lang$core$Maybe$map = F2(
	function (f, maybe) {
		var _p2 = maybe;
		if (_p2.ctor === 'Just') {
			return _elm_lang$core$Maybe$Just(
				f(_p2._0));
		} else {
			return _elm_lang$core$Maybe$Nothing;
		}
	});
var _elm_lang$core$Maybe$map2 = F3(
	function (func, ma, mb) {
		var _p3 = {ctor: '_Tuple2', _0: ma, _1: mb};
		if (((_p3.ctor === '_Tuple2') && (_p3._0.ctor === 'Just')) && (_p3._1.ctor === 'Just')) {
			return _elm_lang$core$Maybe$Just(
				A2(func, _p3._0._0, _p3._1._0));
		} else {
			return _elm_lang$core$Maybe$Nothing;
		}
	});
var _elm_lang$core$Maybe$map3 = F4(
	function (func, ma, mb, mc) {
		var _p4 = {ctor: '_Tuple3', _0: ma, _1: mb, _2: mc};
		if ((((_p4.ctor === '_Tuple3') && (_p4._0.ctor === 'Just')) && (_p4._1.ctor === 'Just')) && (_p4._2.ctor === 'Just')) {
			return _elm_lang$core$Maybe$Just(
				A3(func, _p4._0._0, _p4._1._0, _p4._2._0));
		} else {
			return _elm_lang$core$Maybe$Nothing;
		}
	});
var _elm_lang$core$Maybe$map4 = F5(
	function (func, ma, mb, mc, md) {
		var _p5 = {ctor: '_Tuple4', _0: ma, _1: mb, _2: mc, _3: md};
		if (((((_p5.ctor === '_Tuple4') && (_p5._0.ctor === 'Just')) && (_p5._1.ctor === 'Just')) && (_p5._2.ctor === 'Just')) && (_p5._3.ctor === 'Just')) {
			return _elm_lang$core$Maybe$Just(
				A4(func, _p5._0._0, _p5._1._0, _p5._2._0, _p5._3._0));
		} else {
			return _elm_lang$core$Maybe$Nothing;
		}
	});
var _elm_lang$core$Maybe$map5 = F6(
	function (func, ma, mb, mc, md, me) {
		var _p6 = {ctor: '_Tuple5', _0: ma, _1: mb, _2: mc, _3: md, _4: me};
		if ((((((_p6.ctor === '_Tuple5') && (_p6._0.ctor === 'Just')) && (_p6._1.ctor === 'Just')) && (_p6._2.ctor === 'Just')) && (_p6._3.ctor === 'Just')) && (_p6._4.ctor === 'Just')) {
			return _elm_lang$core$Maybe$Just(
				A5(func, _p6._0._0, _p6._1._0, _p6._2._0, _p6._3._0, _p6._4._0));
		} else {
			return _elm_lang$core$Maybe$Nothing;
		}
	});

//import Native.Utils //

var _elm_lang$core$Native_List = function() {

var Nil = { ctor: '[]' };

function Cons(hd, tl)
{
	return { ctor: '::', _0: hd, _1: tl };
}

function fromArray(arr)
{
	var out = Nil;
	for (var i = arr.length; i--; )
	{
		out = Cons(arr[i], out);
	}
	return out;
}

function toArray(xs)
{
	var out = [];
	while (xs.ctor !== '[]')
	{
		out.push(xs._0);
		xs = xs._1;
	}
	return out;
}

function foldr(f, b, xs)
{
	var arr = toArray(xs);
	var acc = b;
	for (var i = arr.length; i--; )
	{
		acc = A2(f, arr[i], acc);
	}
	return acc;
}

function map2(f, xs, ys)
{
	var arr = [];
	while (xs.ctor !== '[]' && ys.ctor !== '[]')
	{
		arr.push(A2(f, xs._0, ys._0));
		xs = xs._1;
		ys = ys._1;
	}
	return fromArray(arr);
}

function map3(f, xs, ys, zs)
{
	var arr = [];
	while (xs.ctor !== '[]' && ys.ctor !== '[]' && zs.ctor !== '[]')
	{
		arr.push(A3(f, xs._0, ys._0, zs._0));
		xs = xs._1;
		ys = ys._1;
		zs = zs._1;
	}
	return fromArray(arr);
}

function map4(f, ws, xs, ys, zs)
{
	var arr = [];
	while (   ws.ctor !== '[]'
		   && xs.ctor !== '[]'
		   && ys.ctor !== '[]'
		   && zs.ctor !== '[]')
	{
		arr.push(A4(f, ws._0, xs._0, ys._0, zs._0));
		ws = ws._1;
		xs = xs._1;
		ys = ys._1;
		zs = zs._1;
	}
	return fromArray(arr);
}

function map5(f, vs, ws, xs, ys, zs)
{
	var arr = [];
	while (   vs.ctor !== '[]'
		   && ws.ctor !== '[]'
		   && xs.ctor !== '[]'
		   && ys.ctor !== '[]'
		   && zs.ctor !== '[]')
	{
		arr.push(A5(f, vs._0, ws._0, xs._0, ys._0, zs._0));
		vs = vs._1;
		ws = ws._1;
		xs = xs._1;
		ys = ys._1;
		zs = zs._1;
	}
	return fromArray(arr);
}

function sortBy(f, xs)
{
	return fromArray(toArray(xs).sort(function(a, b) {
		return _elm_lang$core$Native_Utils.cmp(f(a), f(b));
	}));
}

function sortWith(f, xs)
{
	return fromArray(toArray(xs).sort(function(a, b) {
		var ord = f(a)(b).ctor;
		return ord === 'EQ' ? 0 : ord === 'LT' ? -1 : 1;
	}));
}

return {
	Nil: Nil,
	Cons: Cons,
	cons: F2(Cons),
	toArray: toArray,
	fromArray: fromArray,

	foldr: F3(foldr),

	map2: F3(map2),
	map3: F4(map3),
	map4: F5(map4),
	map5: F6(map5),
	sortBy: F2(sortBy),
	sortWith: F2(sortWith)
};

}();
var _elm_lang$core$List$sortWith = _elm_lang$core$Native_List.sortWith;
var _elm_lang$core$List$sortBy = _elm_lang$core$Native_List.sortBy;
var _elm_lang$core$List$sort = function (xs) {
	return A2(_elm_lang$core$List$sortBy, _elm_lang$core$Basics$identity, xs);
};
var _elm_lang$core$List$singleton = function (value) {
	return {
		ctor: '::',
		_0: value,
		_1: {ctor: '[]'}
	};
};
var _elm_lang$core$List$drop = F2(
	function (n, list) {
		drop:
		while (true) {
			if (_elm_lang$core$Native_Utils.cmp(n, 0) < 1) {
				return list;
			} else {
				var _p0 = list;
				if (_p0.ctor === '[]') {
					return list;
				} else {
					var _v1 = n - 1,
						_v2 = _p0._1;
					n = _v1;
					list = _v2;
					continue drop;
				}
			}
		}
	});
var _elm_lang$core$List$map5 = _elm_lang$core$Native_List.map5;
var _elm_lang$core$List$map4 = _elm_lang$core$Native_List.map4;
var _elm_lang$core$List$map3 = _elm_lang$core$Native_List.map3;
var _elm_lang$core$List$map2 = _elm_lang$core$Native_List.map2;
var _elm_lang$core$List$any = F2(
	function (isOkay, list) {
		any:
		while (true) {
			var _p1 = list;
			if (_p1.ctor === '[]') {
				return false;
			} else {
				if (isOkay(_p1._0)) {
					return true;
				} else {
					var _v4 = isOkay,
						_v5 = _p1._1;
					isOkay = _v4;
					list = _v5;
					continue any;
				}
			}
		}
	});
var _elm_lang$core$List$all = F2(
	function (isOkay, list) {
		return !A2(
			_elm_lang$core$List$any,
			function (_p2) {
				return !isOkay(_p2);
			},
			list);
	});
var _elm_lang$core$List$foldr = _elm_lang$core$Native_List.foldr;
var _elm_lang$core$List$foldl = F3(
	function (func, acc, list) {
		foldl:
		while (true) {
			var _p3 = list;
			if (_p3.ctor === '[]') {
				return acc;
			} else {
				var _v7 = func,
					_v8 = A2(func, _p3._0, acc),
					_v9 = _p3._1;
				func = _v7;
				acc = _v8;
				list = _v9;
				continue foldl;
			}
		}
	});
var _elm_lang$core$List$length = function (xs) {
	return A3(
		_elm_lang$core$List$foldl,
		F2(
			function (_p4, i) {
				return i + 1;
			}),
		0,
		xs);
};
var _elm_lang$core$List$sum = function (numbers) {
	return A3(
		_elm_lang$core$List$foldl,
		F2(
			function (x, y) {
				return x + y;
			}),
		0,
		numbers);
};
var _elm_lang$core$List$product = function (numbers) {
	return A3(
		_elm_lang$core$List$foldl,
		F2(
			function (x, y) {
				return x * y;
			}),
		1,
		numbers);
};
var _elm_lang$core$List$maximum = function (list) {
	var _p5 = list;
	if (_p5.ctor === '::') {
		return _elm_lang$core$Maybe$Just(
			A3(_elm_lang$core$List$foldl, _elm_lang$core$Basics$max, _p5._0, _p5._1));
	} else {
		return _elm_lang$core$Maybe$Nothing;
	}
};
var _elm_lang$core$List$minimum = function (list) {
	var _p6 = list;
	if (_p6.ctor === '::') {
		return _elm_lang$core$Maybe$Just(
			A3(_elm_lang$core$List$foldl, _elm_lang$core$Basics$min, _p6._0, _p6._1));
	} else {
		return _elm_lang$core$Maybe$Nothing;
	}
};
var _elm_lang$core$List$member = F2(
	function (x, xs) {
		return A2(
			_elm_lang$core$List$any,
			function (a) {
				return _elm_lang$core$Native_Utils.eq(a, x);
			},
			xs);
	});
var _elm_lang$core$List$isEmpty = function (xs) {
	var _p7 = xs;
	if (_p7.ctor === '[]') {
		return true;
	} else {
		return false;
	}
};
var _elm_lang$core$List$tail = function (list) {
	var _p8 = list;
	if (_p8.ctor === '::') {
		return _elm_lang$core$Maybe$Just(_p8._1);
	} else {
		return _elm_lang$core$Maybe$Nothing;
	}
};
var _elm_lang$core$List$head = function (list) {
	var _p9 = list;
	if (_p9.ctor === '::') {
		return _elm_lang$core$Maybe$Just(_p9._0);
	} else {
		return _elm_lang$core$Maybe$Nothing;
	}
};
var _elm_lang$core$List_ops = _elm_lang$core$List_ops || {};
_elm_lang$core$List_ops['::'] = _elm_lang$core$Native_List.cons;
var _elm_lang$core$List$map = F2(
	function (f, xs) {
		return A3(
			_elm_lang$core$List$foldr,
			F2(
				function (x, acc) {
					return {
						ctor: '::',
						_0: f(x),
						_1: acc
					};
				}),
			{ctor: '[]'},
			xs);
	});
var _elm_lang$core$List$filter = F2(
	function (pred, xs) {
		var conditionalCons = F2(
			function (front, back) {
				return pred(front) ? {ctor: '::', _0: front, _1: back} : back;
			});
		return A3(
			_elm_lang$core$List$foldr,
			conditionalCons,
			{ctor: '[]'},
			xs);
	});
var _elm_lang$core$List$maybeCons = F3(
	function (f, mx, xs) {
		var _p10 = f(mx);
		if (_p10.ctor === 'Just') {
			return {ctor: '::', _0: _p10._0, _1: xs};
		} else {
			return xs;
		}
	});
var _elm_lang$core$List$filterMap = F2(
	function (f, xs) {
		return A3(
			_elm_lang$core$List$foldr,
			_elm_lang$core$List$maybeCons(f),
			{ctor: '[]'},
			xs);
	});
var _elm_lang$core$List$reverse = function (list) {
	return A3(
		_elm_lang$core$List$foldl,
		F2(
			function (x, y) {
				return {ctor: '::', _0: x, _1: y};
			}),
		{ctor: '[]'},
		list);
};
var _elm_lang$core$List$scanl = F3(
	function (f, b, xs) {
		var scan1 = F2(
			function (x, accAcc) {
				var _p11 = accAcc;
				if (_p11.ctor === '::') {
					return {
						ctor: '::',
						_0: A2(f, x, _p11._0),
						_1: accAcc
					};
				} else {
					return {ctor: '[]'};
				}
			});
		return _elm_lang$core$List$reverse(
			A3(
				_elm_lang$core$List$foldl,
				scan1,
				{
					ctor: '::',
					_0: b,
					_1: {ctor: '[]'}
				},
				xs));
	});
var _elm_lang$core$List$append = F2(
	function (xs, ys) {
		var _p12 = ys;
		if (_p12.ctor === '[]') {
			return xs;
		} else {
			return A3(
				_elm_lang$core$List$foldr,
				F2(
					function (x, y) {
						return {ctor: '::', _0: x, _1: y};
					}),
				ys,
				xs);
		}
	});
var _elm_lang$core$List$concat = function (lists) {
	return A3(
		_elm_lang$core$List$foldr,
		_elm_lang$core$List$append,
		{ctor: '[]'},
		lists);
};
var _elm_lang$core$List$concatMap = F2(
	function (f, list) {
		return _elm_lang$core$List$concat(
			A2(_elm_lang$core$List$map, f, list));
	});
var _elm_lang$core$List$partition = F2(
	function (pred, list) {
		var step = F2(
			function (x, _p13) {
				var _p14 = _p13;
				var _p16 = _p14._0;
				var _p15 = _p14._1;
				return pred(x) ? {
					ctor: '_Tuple2',
					_0: {ctor: '::', _0: x, _1: _p16},
					_1: _p15
				} : {
					ctor: '_Tuple2',
					_0: _p16,
					_1: {ctor: '::', _0: x, _1: _p15}
				};
			});
		return A3(
			_elm_lang$core$List$foldr,
			step,
			{
				ctor: '_Tuple2',
				_0: {ctor: '[]'},
				_1: {ctor: '[]'}
			},
			list);
	});
var _elm_lang$core$List$unzip = function (pairs) {
	var step = F2(
		function (_p18, _p17) {
			var _p19 = _p18;
			var _p20 = _p17;
			return {
				ctor: '_Tuple2',
				_0: {ctor: '::', _0: _p19._0, _1: _p20._0},
				_1: {ctor: '::', _0: _p19._1, _1: _p20._1}
			};
		});
	return A3(
		_elm_lang$core$List$foldr,
		step,
		{
			ctor: '_Tuple2',
			_0: {ctor: '[]'},
			_1: {ctor: '[]'}
		},
		pairs);
};
var _elm_lang$core$List$intersperse = F2(
	function (sep, xs) {
		var _p21 = xs;
		if (_p21.ctor === '[]') {
			return {ctor: '[]'};
		} else {
			var step = F2(
				function (x, rest) {
					return {
						ctor: '::',
						_0: sep,
						_1: {ctor: '::', _0: x, _1: rest}
					};
				});
			var spersed = A3(
				_elm_lang$core$List$foldr,
				step,
				{ctor: '[]'},
				_p21._1);
			return {ctor: '::', _0: _p21._0, _1: spersed};
		}
	});
var _elm_lang$core$List$takeReverse = F3(
	function (n, list, taken) {
		takeReverse:
		while (true) {
			if (_elm_lang$core$Native_Utils.cmp(n, 0) < 1) {
				return taken;
			} else {
				var _p22 = list;
				if (_p22.ctor === '[]') {
					return taken;
				} else {
					var _v23 = n - 1,
						_v24 = _p22._1,
						_v25 = {ctor: '::', _0: _p22._0, _1: taken};
					n = _v23;
					list = _v24;
					taken = _v25;
					continue takeReverse;
				}
			}
		}
	});
var _elm_lang$core$List$takeTailRec = F2(
	function (n, list) {
		return _elm_lang$core$List$reverse(
			A3(
				_elm_lang$core$List$takeReverse,
				n,
				list,
				{ctor: '[]'}));
	});
var _elm_lang$core$List$takeFast = F3(
	function (ctr, n, list) {
		if (_elm_lang$core$Native_Utils.cmp(n, 0) < 1) {
			return {ctor: '[]'};
		} else {
			var _p23 = {ctor: '_Tuple2', _0: n, _1: list};
			_v26_5:
			do {
				_v26_1:
				do {
					if (_p23.ctor === '_Tuple2') {
						if (_p23._1.ctor === '[]') {
							return list;
						} else {
							if (_p23._1._1.ctor === '::') {
								switch (_p23._0) {
									case 1:
										break _v26_1;
									case 2:
										return {
											ctor: '::',
											_0: _p23._1._0,
											_1: {
												ctor: '::',
												_0: _p23._1._1._0,
												_1: {ctor: '[]'}
											}
										};
									case 3:
										if (_p23._1._1._1.ctor === '::') {
											return {
												ctor: '::',
												_0: _p23._1._0,
												_1: {
													ctor: '::',
													_0: _p23._1._1._0,
													_1: {
														ctor: '::',
														_0: _p23._1._1._1._0,
														_1: {ctor: '[]'}
													}
												}
											};
										} else {
											break _v26_5;
										}
									default:
										if ((_p23._1._1._1.ctor === '::') && (_p23._1._1._1._1.ctor === '::')) {
											var _p28 = _p23._1._1._1._0;
											var _p27 = _p23._1._1._0;
											var _p26 = _p23._1._0;
											var _p25 = _p23._1._1._1._1._0;
											var _p24 = _p23._1._1._1._1._1;
											return (_elm_lang$core$Native_Utils.cmp(ctr, 1000) > 0) ? {
												ctor: '::',
												_0: _p26,
												_1: {
													ctor: '::',
													_0: _p27,
													_1: {
														ctor: '::',
														_0: _p28,
														_1: {
															ctor: '::',
															_0: _p25,
															_1: A2(_elm_lang$core$List$takeTailRec, n - 4, _p24)
														}
													}
												}
											} : {
												ctor: '::',
												_0: _p26,
												_1: {
													ctor: '::',
													_0: _p27,
													_1: {
														ctor: '::',
														_0: _p28,
														_1: {
															ctor: '::',
															_0: _p25,
															_1: A3(_elm_lang$core$List$takeFast, ctr + 1, n - 4, _p24)
														}
													}
												}
											};
										} else {
											break _v26_5;
										}
								}
							} else {
								if (_p23._0 === 1) {
									break _v26_1;
								} else {
									break _v26_5;
								}
							}
						}
					} else {
						break _v26_5;
					}
				} while(false);
				return {
					ctor: '::',
					_0: _p23._1._0,
					_1: {ctor: '[]'}
				};
			} while(false);
			return list;
		}
	});
var _elm_lang$core$List$take = F2(
	function (n, list) {
		return A3(_elm_lang$core$List$takeFast, 0, n, list);
	});
var _elm_lang$core$List$repeatHelp = F3(
	function (result, n, value) {
		repeatHelp:
		while (true) {
			if (_elm_lang$core$Native_Utils.cmp(n, 0) < 1) {
				return result;
			} else {
				var _v27 = {ctor: '::', _0: value, _1: result},
					_v28 = n - 1,
					_v29 = value;
				result = _v27;
				n = _v28;
				value = _v29;
				continue repeatHelp;
			}
		}
	});
var _elm_lang$core$List$repeat = F2(
	function (n, value) {
		return A3(
			_elm_lang$core$List$repeatHelp,
			{ctor: '[]'},
			n,
			value);
	});
var _elm_lang$core$List$rangeHelp = F3(
	function (lo, hi, list) {
		rangeHelp:
		while (true) {
			if (_elm_lang$core$Native_Utils.cmp(lo, hi) < 1) {
				var _v30 = lo,
					_v31 = hi - 1,
					_v32 = {ctor: '::', _0: hi, _1: list};
				lo = _v30;
				hi = _v31;
				list = _v32;
				continue rangeHelp;
			} else {
				return list;
			}
		}
	});
var _elm_lang$core$List$range = F2(
	function (lo, hi) {
		return A3(
			_elm_lang$core$List$rangeHelp,
			lo,
			hi,
			{ctor: '[]'});
	});
var _elm_lang$core$List$indexedMap = F2(
	function (f, xs) {
		return A3(
			_elm_lang$core$List$map2,
			f,
			A2(
				_elm_lang$core$List$range,
				0,
				_elm_lang$core$List$length(xs) - 1),
			xs);
	});

var _elm_lang$core$Result$toMaybe = function (result) {
	var _p0 = result;
	if (_p0.ctor === 'Ok') {
		return _elm_lang$core$Maybe$Just(_p0._0);
	} else {
		return _elm_lang$core$Maybe$Nothing;
	}
};
var _elm_lang$core$Result$withDefault = F2(
	function (def, result) {
		var _p1 = result;
		if (_p1.ctor === 'Ok') {
			return _p1._0;
		} else {
			return def;
		}
	});
var _elm_lang$core$Result$Err = function (a) {
	return {ctor: 'Err', _0: a};
};
var _elm_lang$core$Result$andThen = F2(
	function (callback, result) {
		var _p2 = result;
		if (_p2.ctor === 'Ok') {
			return callback(_p2._0);
		} else {
			return _elm_lang$core$Result$Err(_p2._0);
		}
	});
var _elm_lang$core$Result$Ok = function (a) {
	return {ctor: 'Ok', _0: a};
};
var _elm_lang$core$Result$map = F2(
	function (func, ra) {
		var _p3 = ra;
		if (_p3.ctor === 'Ok') {
			return _elm_lang$core$Result$Ok(
				func(_p3._0));
		} else {
			return _elm_lang$core$Result$Err(_p3._0);
		}
	});
var _elm_lang$core$Result$map2 = F3(
	function (func, ra, rb) {
		var _p4 = {ctor: '_Tuple2', _0: ra, _1: rb};
		if (_p4._0.ctor === 'Ok') {
			if (_p4._1.ctor === 'Ok') {
				return _elm_lang$core$Result$Ok(
					A2(func, _p4._0._0, _p4._1._0));
			} else {
				return _elm_lang$core$Result$Err(_p4._1._0);
			}
		} else {
			return _elm_lang$core$Result$Err(_p4._0._0);
		}
	});
var _elm_lang$core$Result$map3 = F4(
	function (func, ra, rb, rc) {
		var _p5 = {ctor: '_Tuple3', _0: ra, _1: rb, _2: rc};
		if (_p5._0.ctor === 'Ok') {
			if (_p5._1.ctor === 'Ok') {
				if (_p5._2.ctor === 'Ok') {
					return _elm_lang$core$Result$Ok(
						A3(func, _p5._0._0, _p5._1._0, _p5._2._0));
				} else {
					return _elm_lang$core$Result$Err(_p5._2._0);
				}
			} else {
				return _elm_lang$core$Result$Err(_p5._1._0);
			}
		} else {
			return _elm_lang$core$Result$Err(_p5._0._0);
		}
	});
var _elm_lang$core$Result$map4 = F5(
	function (func, ra, rb, rc, rd) {
		var _p6 = {ctor: '_Tuple4', _0: ra, _1: rb, _2: rc, _3: rd};
		if (_p6._0.ctor === 'Ok') {
			if (_p6._1.ctor === 'Ok') {
				if (_p6._2.ctor === 'Ok') {
					if (_p6._3.ctor === 'Ok') {
						return _elm_lang$core$Result$Ok(
							A4(func, _p6._0._0, _p6._1._0, _p6._2._0, _p6._3._0));
					} else {
						return _elm_lang$core$Result$Err(_p6._3._0);
					}
				} else {
					return _elm_lang$core$Result$Err(_p6._2._0);
				}
			} else {
				return _elm_lang$core$Result$Err(_p6._1._0);
			}
		} else {
			return _elm_lang$core$Result$Err(_p6._0._0);
		}
	});
var _elm_lang$core$Result$map5 = F6(
	function (func, ra, rb, rc, rd, re) {
		var _p7 = {ctor: '_Tuple5', _0: ra, _1: rb, _2: rc, _3: rd, _4: re};
		if (_p7._0.ctor === 'Ok') {
			if (_p7._1.ctor === 'Ok') {
				if (_p7._2.ctor === 'Ok') {
					if (_p7._3.ctor === 'Ok') {
						if (_p7._4.ctor === 'Ok') {
							return _elm_lang$core$Result$Ok(
								A5(func, _p7._0._0, _p7._1._0, _p7._2._0, _p7._3._0, _p7._4._0));
						} else {
							return _elm_lang$core$Result$Err(_p7._4._0);
						}
					} else {
						return _elm_lang$core$Result$Err(_p7._3._0);
					}
				} else {
					return _elm_lang$core$Result$Err(_p7._2._0);
				}
			} else {
				return _elm_lang$core$Result$Err(_p7._1._0);
			}
		} else {
			return _elm_lang$core$Result$Err(_p7._0._0);
		}
	});
var _elm_lang$core$Result$mapError = F2(
	function (f, result) {
		var _p8 = result;
		if (_p8.ctor === 'Ok') {
			return _elm_lang$core$Result$Ok(_p8._0);
		} else {
			return _elm_lang$core$Result$Err(
				f(_p8._0));
		}
	});
var _elm_lang$core$Result$fromMaybe = F2(
	function (err, maybe) {
		var _p9 = maybe;
		if (_p9.ctor === 'Just') {
			return _elm_lang$core$Result$Ok(_p9._0);
		} else {
			return _elm_lang$core$Result$Err(err);
		}
	});

//import Maybe, Native.List, Native.Utils, Result //

var _elm_lang$core$Native_String = function() {

function isEmpty(str)
{
	return str.length === 0;
}
function cons(chr, str)
{
	return chr + str;
}
function uncons(str)
{
	var hd = str[0];
	if (hd)
	{
		return _elm_lang$core$Maybe$Just(_elm_lang$core$Native_Utils.Tuple2(_elm_lang$core$Native_Utils.chr(hd), str.slice(1)));
	}
	return _elm_lang$core$Maybe$Nothing;
}
function append(a, b)
{
	return a + b;
}
function concat(strs)
{
	return _elm_lang$core$Native_List.toArray(strs).join('');
}
function length(str)
{
	return str.length;
}
function map(f, str)
{
	var out = str.split('');
	for (var i = out.length; i--; )
	{
		out[i] = f(_elm_lang$core$Native_Utils.chr(out[i]));
	}
	return out.join('');
}
function filter(pred, str)
{
	return str.split('').map(_elm_lang$core$Native_Utils.chr).filter(pred).join('');
}
function reverse(str)
{
	return str.split('').reverse().join('');
}
function foldl(f, b, str)
{
	var len = str.length;
	for (var i = 0; i < len; ++i)
	{
		b = A2(f, _elm_lang$core$Native_Utils.chr(str[i]), b);
	}
	return b;
}
function foldr(f, b, str)
{
	for (var i = str.length; i--; )
	{
		b = A2(f, _elm_lang$core$Native_Utils.chr(str[i]), b);
	}
	return b;
}
function split(sep, str)
{
	return _elm_lang$core$Native_List.fromArray(str.split(sep));
}
function join(sep, strs)
{
	return _elm_lang$core$Native_List.toArray(strs).join(sep);
}
function repeat(n, str)
{
	var result = '';
	while (n > 0)
	{
		if (n & 1)
		{
			result += str;
		}
		n >>= 1, str += str;
	}
	return result;
}
function slice(start, end, str)
{
	return str.slice(start, end);
}
function left(n, str)
{
	return n < 1 ? '' : str.slice(0, n);
}
function right(n, str)
{
	return n < 1 ? '' : str.slice(-n);
}
function dropLeft(n, str)
{
	return n < 1 ? str : str.slice(n);
}
function dropRight(n, str)
{
	return n < 1 ? str : str.slice(0, -n);
}
function pad(n, chr, str)
{
	var half = (n - str.length) / 2;
	return repeat(Math.ceil(half), chr) + str + repeat(half | 0, chr);
}
function padRight(n, chr, str)
{
	return str + repeat(n - str.length, chr);
}
function padLeft(n, chr, str)
{
	return repeat(n - str.length, chr) + str;
}

function trim(str)
{
	return str.trim();
}
function trimLeft(str)
{
	return str.replace(/^\s+/, '');
}
function trimRight(str)
{
	return str.replace(/\s+$/, '');
}

function words(str)
{
	return _elm_lang$core$Native_List.fromArray(str.trim().split(/\s+/g));
}
function lines(str)
{
	return _elm_lang$core$Native_List.fromArray(str.split(/\r\n|\r|\n/g));
}

function toUpper(str)
{
	return str.toUpperCase();
}
function toLower(str)
{
	return str.toLowerCase();
}

function any(pred, str)
{
	for (var i = str.length; i--; )
	{
		if (pred(_elm_lang$core$Native_Utils.chr(str[i])))
		{
			return true;
		}
	}
	return false;
}
function all(pred, str)
{
	for (var i = str.length; i--; )
	{
		if (!pred(_elm_lang$core$Native_Utils.chr(str[i])))
		{
			return false;
		}
	}
	return true;
}

function contains(sub, str)
{
	return str.indexOf(sub) > -1;
}
function startsWith(sub, str)
{
	return str.indexOf(sub) === 0;
}
function endsWith(sub, str)
{
	return str.length >= sub.length &&
		str.lastIndexOf(sub) === str.length - sub.length;
}
function indexes(sub, str)
{
	var subLen = sub.length;

	if (subLen < 1)
	{
		return _elm_lang$core$Native_List.Nil;
	}

	var i = 0;
	var is = [];

	while ((i = str.indexOf(sub, i)) > -1)
	{
		is.push(i);
		i = i + subLen;
	}

	return _elm_lang$core$Native_List.fromArray(is);
}


function toInt(s)
{
	var len = s.length;

	// if empty
	if (len === 0)
	{
		return intErr(s);
	}

	// if hex
	var c = s[0];
	if (c === '0' && s[1] === 'x')
	{
		for (var i = 2; i < len; ++i)
		{
			var c = s[i];
			if (('0' <= c && c <= '9') || ('A' <= c && c <= 'F') || ('a' <= c && c <= 'f'))
			{
				continue;
			}
			return intErr(s);
		}
		return _elm_lang$core$Result$Ok(parseInt(s, 16));
	}

	// is decimal
	if (c > '9' || (c < '0' && c !== '-' && c !== '+'))
	{
		return intErr(s);
	}
	for (var i = 1; i < len; ++i)
	{
		var c = s[i];
		if (c < '0' || '9' < c)
		{
			return intErr(s);
		}
	}

	return _elm_lang$core$Result$Ok(parseInt(s, 10));
}

function intErr(s)
{
	return _elm_lang$core$Result$Err("could not convert string '" + s + "' to an Int");
}


function toFloat(s)
{
	// check if it is a hex, octal, or binary number
	if (s.length === 0 || /[\sxbo]/.test(s))
	{
		return floatErr(s);
	}
	var n = +s;
	// faster isNaN check
	return n === n ? _elm_lang$core$Result$Ok(n) : floatErr(s);
}

function floatErr(s)
{
	return _elm_lang$core$Result$Err("could not convert string '" + s + "' to a Float");
}


function toList(str)
{
	return _elm_lang$core$Native_List.fromArray(str.split('').map(_elm_lang$core$Native_Utils.chr));
}
function fromList(chars)
{
	return _elm_lang$core$Native_List.toArray(chars).join('');
}

return {
	isEmpty: isEmpty,
	cons: F2(cons),
	uncons: uncons,
	append: F2(append),
	concat: concat,
	length: length,
	map: F2(map),
	filter: F2(filter),
	reverse: reverse,
	foldl: F3(foldl),
	foldr: F3(foldr),

	split: F2(split),
	join: F2(join),
	repeat: F2(repeat),

	slice: F3(slice),
	left: F2(left),
	right: F2(right),
	dropLeft: F2(dropLeft),
	dropRight: F2(dropRight),

	pad: F3(pad),
	padLeft: F3(padLeft),
	padRight: F3(padRight),

	trim: trim,
	trimLeft: trimLeft,
	trimRight: trimRight,

	words: words,
	lines: lines,

	toUpper: toUpper,
	toLower: toLower,

	any: F2(any),
	all: F2(all),

	contains: F2(contains),
	startsWith: F2(startsWith),
	endsWith: F2(endsWith),
	indexes: F2(indexes),

	toInt: toInt,
	toFloat: toFloat,
	toList: toList,
	fromList: fromList
};

}();

//import Native.Utils //

var _elm_lang$core$Native_Char = function() {

return {
	fromCode: function(c) { return _elm_lang$core$Native_Utils.chr(String.fromCharCode(c)); },
	toCode: function(c) { return c.charCodeAt(0); },
	toUpper: function(c) { return _elm_lang$core$Native_Utils.chr(c.toUpperCase()); },
	toLower: function(c) { return _elm_lang$core$Native_Utils.chr(c.toLowerCase()); },
	toLocaleUpper: function(c) { return _elm_lang$core$Native_Utils.chr(c.toLocaleUpperCase()); },
	toLocaleLower: function(c) { return _elm_lang$core$Native_Utils.chr(c.toLocaleLowerCase()); }
};

}();
var _elm_lang$core$Char$fromCode = _elm_lang$core$Native_Char.fromCode;
var _elm_lang$core$Char$toCode = _elm_lang$core$Native_Char.toCode;
var _elm_lang$core$Char$toLocaleLower = _elm_lang$core$Native_Char.toLocaleLower;
var _elm_lang$core$Char$toLocaleUpper = _elm_lang$core$Native_Char.toLocaleUpper;
var _elm_lang$core$Char$toLower = _elm_lang$core$Native_Char.toLower;
var _elm_lang$core$Char$toUpper = _elm_lang$core$Native_Char.toUpper;
var _elm_lang$core$Char$isBetween = F3(
	function (low, high, $char) {
		var code = _elm_lang$core$Char$toCode($char);
		return (_elm_lang$core$Native_Utils.cmp(
			code,
			_elm_lang$core$Char$toCode(low)) > -1) && (_elm_lang$core$Native_Utils.cmp(
			code,
			_elm_lang$core$Char$toCode(high)) < 1);
	});
var _elm_lang$core$Char$isUpper = A2(
	_elm_lang$core$Char$isBetween,
	_elm_lang$core$Native_Utils.chr('A'),
	_elm_lang$core$Native_Utils.chr('Z'));
var _elm_lang$core$Char$isLower = A2(
	_elm_lang$core$Char$isBetween,
	_elm_lang$core$Native_Utils.chr('a'),
	_elm_lang$core$Native_Utils.chr('z'));
var _elm_lang$core$Char$isDigit = A2(
	_elm_lang$core$Char$isBetween,
	_elm_lang$core$Native_Utils.chr('0'),
	_elm_lang$core$Native_Utils.chr('9'));
var _elm_lang$core$Char$isOctDigit = A2(
	_elm_lang$core$Char$isBetween,
	_elm_lang$core$Native_Utils.chr('0'),
	_elm_lang$core$Native_Utils.chr('7'));
var _elm_lang$core$Char$isHexDigit = function ($char) {
	return _elm_lang$core$Char$isDigit($char) || (A3(
		_elm_lang$core$Char$isBetween,
		_elm_lang$core$Native_Utils.chr('a'),
		_elm_lang$core$Native_Utils.chr('f'),
		$char) || A3(
		_elm_lang$core$Char$isBetween,
		_elm_lang$core$Native_Utils.chr('A'),
		_elm_lang$core$Native_Utils.chr('F'),
		$char));
};

var _elm_lang$core$String$fromList = _elm_lang$core$Native_String.fromList;
var _elm_lang$core$String$toList = _elm_lang$core$Native_String.toList;
var _elm_lang$core$String$toFloat = _elm_lang$core$Native_String.toFloat;
var _elm_lang$core$String$toInt = _elm_lang$core$Native_String.toInt;
var _elm_lang$core$String$indices = _elm_lang$core$Native_String.indexes;
var _elm_lang$core$String$indexes = _elm_lang$core$Native_String.indexes;
var _elm_lang$core$String$endsWith = _elm_lang$core$Native_String.endsWith;
var _elm_lang$core$String$startsWith = _elm_lang$core$Native_String.startsWith;
var _elm_lang$core$String$contains = _elm_lang$core$Native_String.contains;
var _elm_lang$core$String$all = _elm_lang$core$Native_String.all;
var _elm_lang$core$String$any = _elm_lang$core$Native_String.any;
var _elm_lang$core$String$toLower = _elm_lang$core$Native_String.toLower;
var _elm_lang$core$String$toUpper = _elm_lang$core$Native_String.toUpper;
var _elm_lang$core$String$lines = _elm_lang$core$Native_String.lines;
var _elm_lang$core$String$words = _elm_lang$core$Native_String.words;
var _elm_lang$core$String$trimRight = _elm_lang$core$Native_String.trimRight;
var _elm_lang$core$String$trimLeft = _elm_lang$core$Native_String.trimLeft;
var _elm_lang$core$String$trim = _elm_lang$core$Native_String.trim;
var _elm_lang$core$String$padRight = _elm_lang$core$Native_String.padRight;
var _elm_lang$core$String$padLeft = _elm_lang$core$Native_String.padLeft;
var _elm_lang$core$String$pad = _elm_lang$core$Native_String.pad;
var _elm_lang$core$String$dropRight = _elm_lang$core$Native_String.dropRight;
var _elm_lang$core$String$dropLeft = _elm_lang$core$Native_String.dropLeft;
var _elm_lang$core$String$right = _elm_lang$core$Native_String.right;
var _elm_lang$core$String$left = _elm_lang$core$Native_String.left;
var _elm_lang$core$String$slice = _elm_lang$core$Native_String.slice;
var _elm_lang$core$String$repeat = _elm_lang$core$Native_String.repeat;
var _elm_lang$core$String$join = _elm_lang$core$Native_String.join;
var _elm_lang$core$String$split = _elm_lang$core$Native_String.split;
var _elm_lang$core$String$foldr = _elm_lang$core$Native_String.foldr;
var _elm_lang$core$String$foldl = _elm_lang$core$Native_String.foldl;
var _elm_lang$core$String$reverse = _elm_lang$core$Native_String.reverse;
var _elm_lang$core$String$filter = _elm_lang$core$Native_String.filter;
var _elm_lang$core$String$map = _elm_lang$core$Native_String.map;
var _elm_lang$core$String$length = _elm_lang$core$Native_String.length;
var _elm_lang$core$String$concat = _elm_lang$core$Native_String.concat;
var _elm_lang$core$String$append = _elm_lang$core$Native_String.append;
var _elm_lang$core$String$uncons = _elm_lang$core$Native_String.uncons;
var _elm_lang$core$String$cons = _elm_lang$core$Native_String.cons;
var _elm_lang$core$String$fromChar = function ($char) {
	return A2(_elm_lang$core$String$cons, $char, '');
};
var _elm_lang$core$String$isEmpty = _elm_lang$core$Native_String.isEmpty;

var _elm_lang$core$Tuple$mapSecond = F2(
	function (func, _p0) {
		var _p1 = _p0;
		return {
			ctor: '_Tuple2',
			_0: _p1._0,
			_1: func(_p1._1)
		};
	});
var _elm_lang$core$Tuple$mapFirst = F2(
	function (func, _p2) {
		var _p3 = _p2;
		return {
			ctor: '_Tuple2',
			_0: func(_p3._0),
			_1: _p3._1
		};
	});
var _elm_lang$core$Tuple$second = function (_p4) {
	var _p5 = _p4;
	return _p5._1;
};
var _elm_lang$core$Tuple$first = function (_p6) {
	var _p7 = _p6;
	return _p7._0;
};

//import //

var _elm_lang$core$Native_Platform = function() {


// PROGRAMS

function program(impl)
{
	return function(flagDecoder)
	{
		return function(object, moduleName)
		{
			object['worker'] = function worker(flags)
			{
				if (typeof flags !== 'undefined')
				{
					throw new Error(
						'The `' + moduleName + '` module does not need flags.\n'
						+ 'Call ' + moduleName + '.worker() with no arguments and you should be all set!'
					);
				}

				return initialize(
					impl.init,
					impl.update,
					impl.subscriptions,
					renderer
				);
			};
		};
	};
}

function programWithFlags(impl)
{
	return function(flagDecoder)
	{
		return function(object, moduleName)
		{
			object['worker'] = function worker(flags)
			{
				if (typeof flagDecoder === 'undefined')
				{
					throw new Error(
						'Are you trying to sneak a Never value into Elm? Trickster!\n'
						+ 'It looks like ' + moduleName + '.main is defined with `programWithFlags` but has type `Program Never`.\n'
						+ 'Use `program` instead if you do not want flags.'
					);
				}

				var result = A2(_elm_lang$core$Native_Json.run, flagDecoder, flags);
				if (result.ctor === 'Err')
				{
					throw new Error(
						moduleName + '.worker(...) was called with an unexpected argument.\n'
						+ 'I tried to convert it to an Elm value, but ran into this problem:\n\n'
						+ result._0
					);
				}

				return initialize(
					impl.init(result._0),
					impl.update,
					impl.subscriptions,
					renderer
				);
			};
		};
	};
}

function renderer(enqueue, _)
{
	return function(_) {};
}


// HTML TO PROGRAM

function htmlToProgram(vnode)
{
	var emptyBag = batch(_elm_lang$core$Native_List.Nil);
	var noChange = _elm_lang$core$Native_Utils.Tuple2(
		_elm_lang$core$Native_Utils.Tuple0,
		emptyBag
	);

	return _elm_lang$virtual_dom$VirtualDom$program({
		init: noChange,
		view: function(model) { return main; },
		update: F2(function(msg, model) { return noChange; }),
		subscriptions: function (model) { return emptyBag; }
	});
}


// INITIALIZE A PROGRAM

function initialize(init, update, subscriptions, renderer)
{
	// ambient state
	var managers = {};
	var updateView;

	// init and update state in main process
	var initApp = _elm_lang$core$Native_Scheduler.nativeBinding(function(callback) {
		var model = init._0;
		updateView = renderer(enqueue, model);
		var cmds = init._1;
		var subs = subscriptions(model);
		dispatchEffects(managers, cmds, subs);
		callback(_elm_lang$core$Native_Scheduler.succeed(model));
	});

	function onMessage(msg, model)
	{
		return _elm_lang$core$Native_Scheduler.nativeBinding(function(callback) {
			var results = A2(update, msg, model);
			model = results._0;
			updateView(model);
			var cmds = results._1;
			var subs = subscriptions(model);
			dispatchEffects(managers, cmds, subs);
			callback(_elm_lang$core$Native_Scheduler.succeed(model));
		});
	}

	var mainProcess = spawnLoop(initApp, onMessage);

	function enqueue(msg)
	{
		_elm_lang$core$Native_Scheduler.rawSend(mainProcess, msg);
	}

	var ports = setupEffects(managers, enqueue);

	return ports ? { ports: ports } : {};
}


// EFFECT MANAGERS

var effectManagers = {};

function setupEffects(managers, callback)
{
	var ports;

	// setup all necessary effect managers
	for (var key in effectManagers)
	{
		var manager = effectManagers[key];

		if (manager.isForeign)
		{
			ports = ports || {};
			ports[key] = manager.tag === 'cmd'
				? setupOutgoingPort(key)
				: setupIncomingPort(key, callback);
		}

		managers[key] = makeManager(manager, callback);
	}

	return ports;
}

function makeManager(info, callback)
{
	var router = {
		main: callback,
		self: undefined
	};

	var tag = info.tag;
	var onEffects = info.onEffects;
	var onSelfMsg = info.onSelfMsg;

	function onMessage(msg, state)
	{
		if (msg.ctor === 'self')
		{
			return A3(onSelfMsg, router, msg._0, state);
		}

		var fx = msg._0;
		switch (tag)
		{
			case 'cmd':
				return A3(onEffects, router, fx.cmds, state);

			case 'sub':
				return A3(onEffects, router, fx.subs, state);

			case 'fx':
				return A4(onEffects, router, fx.cmds, fx.subs, state);
		}
	}

	var process = spawnLoop(info.init, onMessage);
	router.self = process;
	return process;
}

function sendToApp(router, msg)
{
	return _elm_lang$core$Native_Scheduler.nativeBinding(function(callback)
	{
		router.main(msg);
		callback(_elm_lang$core$Native_Scheduler.succeed(_elm_lang$core$Native_Utils.Tuple0));
	});
}

function sendToSelf(router, msg)
{
	return A2(_elm_lang$core$Native_Scheduler.send, router.self, {
		ctor: 'self',
		_0: msg
	});
}


// HELPER for STATEFUL LOOPS

function spawnLoop(init, onMessage)
{
	var andThen = _elm_lang$core$Native_Scheduler.andThen;

	function loop(state)
	{
		var handleMsg = _elm_lang$core$Native_Scheduler.receive(function(msg) {
			return onMessage(msg, state);
		});
		return A2(andThen, loop, handleMsg);
	}

	var task = A2(andThen, loop, init);

	return _elm_lang$core$Native_Scheduler.rawSpawn(task);
}


// BAGS

function leaf(home)
{
	return function(value)
	{
		return {
			type: 'leaf',
			home: home,
			value: value
		};
	};
}

function batch(list)
{
	return {
		type: 'node',
		branches: list
	};
}

function map(tagger, bag)
{
	return {
		type: 'map',
		tagger: tagger,
		tree: bag
	}
}


// PIPE BAGS INTO EFFECT MANAGERS

function dispatchEffects(managers, cmdBag, subBag)
{
	var effectsDict = {};
	gatherEffects(true, cmdBag, effectsDict, null);
	gatherEffects(false, subBag, effectsDict, null);

	for (var home in managers)
	{
		var fx = home in effectsDict
			? effectsDict[home]
			: {
				cmds: _elm_lang$core$Native_List.Nil,
				subs: _elm_lang$core$Native_List.Nil
			};

		_elm_lang$core$Native_Scheduler.rawSend(managers[home], { ctor: 'fx', _0: fx });
	}
}

function gatherEffects(isCmd, bag, effectsDict, taggers)
{
	switch (bag.type)
	{
		case 'leaf':
			var home = bag.home;
			var effect = toEffect(isCmd, home, taggers, bag.value);
			effectsDict[home] = insert(isCmd, effect, effectsDict[home]);
			return;

		case 'node':
			var list = bag.branches;
			while (list.ctor !== '[]')
			{
				gatherEffects(isCmd, list._0, effectsDict, taggers);
				list = list._1;
			}
			return;

		case 'map':
			gatherEffects(isCmd, bag.tree, effectsDict, {
				tagger: bag.tagger,
				rest: taggers
			});
			return;
	}
}

function toEffect(isCmd, home, taggers, value)
{
	function applyTaggers(x)
	{
		var temp = taggers;
		while (temp)
		{
			x = temp.tagger(x);
			temp = temp.rest;
		}
		return x;
	}

	var map = isCmd
		? effectManagers[home].cmdMap
		: effectManagers[home].subMap;

	return A2(map, applyTaggers, value)
}

function insert(isCmd, newEffect, effects)
{
	effects = effects || {
		cmds: _elm_lang$core$Native_List.Nil,
		subs: _elm_lang$core$Native_List.Nil
	};
	if (isCmd)
	{
		effects.cmds = _elm_lang$core$Native_List.Cons(newEffect, effects.cmds);
		return effects;
	}
	effects.subs = _elm_lang$core$Native_List.Cons(newEffect, effects.subs);
	return effects;
}


// PORTS

function checkPortName(name)
{
	if (name in effectManagers)
	{
		throw new Error('There can only be one port named `' + name + '`, but your program has multiple.');
	}
}


// OUTGOING PORTS

function outgoingPort(name, converter)
{
	checkPortName(name);
	effectManagers[name] = {
		tag: 'cmd',
		cmdMap: outgoingPortMap,
		converter: converter,
		isForeign: true
	};
	return leaf(name);
}

var outgoingPortMap = F2(function cmdMap(tagger, value) {
	return value;
});

function setupOutgoingPort(name)
{
	var subs = [];
	var converter = effectManagers[name].converter;

	// CREATE MANAGER

	var init = _elm_lang$core$Native_Scheduler.succeed(null);

	function onEffects(router, cmdList, state)
	{
		while (cmdList.ctor !== '[]')
		{
			// grab a separate reference to subs in case unsubscribe is called
			var currentSubs = subs;
			var value = converter(cmdList._0);
			for (var i = 0; i < currentSubs.length; i++)
			{
				currentSubs[i](value);
			}
			cmdList = cmdList._1;
		}
		return init;
	}

	effectManagers[name].init = init;
	effectManagers[name].onEffects = F3(onEffects);

	// PUBLIC API

	function subscribe(callback)
	{
		subs.push(callback);
	}

	function unsubscribe(callback)
	{
		// copy subs into a new array in case unsubscribe is called within a
		// subscribed callback
		subs = subs.slice();
		var index = subs.indexOf(callback);
		if (index >= 0)
		{
			subs.splice(index, 1);
		}
	}

	return {
		subscribe: subscribe,
		unsubscribe: unsubscribe
	};
}


// INCOMING PORTS

function incomingPort(name, converter)
{
	checkPortName(name);
	effectManagers[name] = {
		tag: 'sub',
		subMap: incomingPortMap,
		converter: converter,
		isForeign: true
	};
	return leaf(name);
}

var incomingPortMap = F2(function subMap(tagger, finalTagger)
{
	return function(value)
	{
		return tagger(finalTagger(value));
	};
});

function setupIncomingPort(name, callback)
{
	var sentBeforeInit = [];
	var subs = _elm_lang$core$Native_List.Nil;
	var converter = effectManagers[name].converter;
	var currentOnEffects = preInitOnEffects;
	var currentSend = preInitSend;

	// CREATE MANAGER

	var init = _elm_lang$core$Native_Scheduler.succeed(null);

	function preInitOnEffects(router, subList, state)
	{
		var postInitResult = postInitOnEffects(router, subList, state);

		for(var i = 0; i < sentBeforeInit.length; i++)
		{
			postInitSend(sentBeforeInit[i]);
		}

		sentBeforeInit = null; // to release objects held in queue
		currentSend = postInitSend;
		currentOnEffects = postInitOnEffects;
		return postInitResult;
	}

	function postInitOnEffects(router, subList, state)
	{
		subs = subList;
		return init;
	}

	function onEffects(router, subList, state)
	{
		return currentOnEffects(router, subList, state);
	}

	effectManagers[name].init = init;
	effectManagers[name].onEffects = F3(onEffects);

	// PUBLIC API

	function preInitSend(value)
	{
		sentBeforeInit.push(value);
	}

	function postInitSend(value)
	{
		var temp = subs;
		while (temp.ctor !== '[]')
		{
			callback(temp._0(value));
			temp = temp._1;
		}
	}

	function send(incomingValue)
	{
		var result = A2(_elm_lang$core$Json_Decode$decodeValue, converter, incomingValue);
		if (result.ctor === 'Err')
		{
			throw new Error('Trying to send an unexpected type of value through port `' + name + '`:\n' + result._0);
		}

		currentSend(result._0);
	}

	return { send: send };
}

return {
	// routers
	sendToApp: F2(sendToApp),
	sendToSelf: F2(sendToSelf),

	// global setup
	effectManagers: effectManagers,
	outgoingPort: outgoingPort,
	incomingPort: incomingPort,

	htmlToProgram: htmlToProgram,
	program: program,
	programWithFlags: programWithFlags,
	initialize: initialize,

	// effect bags
	leaf: leaf,
	batch: batch,
	map: F2(map)
};

}();

//import Native.Utils //

var _elm_lang$core$Native_Scheduler = function() {

var MAX_STEPS = 10000;


// TASKS

function succeed(value)
{
	return {
		ctor: '_Task_succeed',
		value: value
	};
}

function fail(error)
{
	return {
		ctor: '_Task_fail',
		value: error
	};
}

function nativeBinding(callback)
{
	return {
		ctor: '_Task_nativeBinding',
		callback: callback,
		cancel: null
	};
}

function andThen(callback, task)
{
	return {
		ctor: '_Task_andThen',
		callback: callback,
		task: task
	};
}

function onError(callback, task)
{
	return {
		ctor: '_Task_onError',
		callback: callback,
		task: task
	};
}

function receive(callback)
{
	return {
		ctor: '_Task_receive',
		callback: callback
	};
}


// PROCESSES

function rawSpawn(task)
{
	var process = {
		ctor: '_Process',
		id: _elm_lang$core$Native_Utils.guid(),
		root: task,
		stack: null,
		mailbox: []
	};

	enqueue(process);

	return process;
}

function spawn(task)
{
	return nativeBinding(function(callback) {
		var process = rawSpawn(task);
		callback(succeed(process));
	});
}

function rawSend(process, msg)
{
	process.mailbox.push(msg);
	enqueue(process);
}

function send(process, msg)
{
	return nativeBinding(function(callback) {
		rawSend(process, msg);
		callback(succeed(_elm_lang$core$Native_Utils.Tuple0));
	});
}

function kill(process)
{
	return nativeBinding(function(callback) {
		var root = process.root;
		if (root.ctor === '_Task_nativeBinding' && root.cancel)
		{
			root.cancel();
		}

		process.root = null;

		callback(succeed(_elm_lang$core$Native_Utils.Tuple0));
	});
}

function sleep(time)
{
	return nativeBinding(function(callback) {
		var id = setTimeout(function() {
			callback(succeed(_elm_lang$core$Native_Utils.Tuple0));
		}, time);

		return function() { clearTimeout(id); };
	});
}


// STEP PROCESSES

function step(numSteps, process)
{
	while (numSteps < MAX_STEPS)
	{
		var ctor = process.root.ctor;

		if (ctor === '_Task_succeed')
		{
			while (process.stack && process.stack.ctor === '_Task_onError')
			{
				process.stack = process.stack.rest;
			}
			if (process.stack === null)
			{
				break;
			}
			process.root = process.stack.callback(process.root.value);
			process.stack = process.stack.rest;
			++numSteps;
			continue;
		}

		if (ctor === '_Task_fail')
		{
			while (process.stack && process.stack.ctor === '_Task_andThen')
			{
				process.stack = process.stack.rest;
			}
			if (process.stack === null)
			{
				break;
			}
			process.root = process.stack.callback(process.root.value);
			process.stack = process.stack.rest;
			++numSteps;
			continue;
		}

		if (ctor === '_Task_andThen')
		{
			process.stack = {
				ctor: '_Task_andThen',
				callback: process.root.callback,
				rest: process.stack
			};
			process.root = process.root.task;
			++numSteps;
			continue;
		}

		if (ctor === '_Task_onError')
		{
			process.stack = {
				ctor: '_Task_onError',
				callback: process.root.callback,
				rest: process.stack
			};
			process.root = process.root.task;
			++numSteps;
			continue;
		}

		if (ctor === '_Task_nativeBinding')
		{
			process.root.cancel = process.root.callback(function(newRoot) {
				process.root = newRoot;
				enqueue(process);
			});

			break;
		}

		if (ctor === '_Task_receive')
		{
			var mailbox = process.mailbox;
			if (mailbox.length === 0)
			{
				break;
			}

			process.root = process.root.callback(mailbox.shift());
			++numSteps;
			continue;
		}

		throw new Error(ctor);
	}

	if (numSteps < MAX_STEPS)
	{
		return numSteps + 1;
	}
	enqueue(process);

	return numSteps;
}


// WORK QUEUE

var working = false;
var workQueue = [];

function enqueue(process)
{
	workQueue.push(process);

	if (!working)
	{
		setTimeout(work, 0);
		working = true;
	}
}

function work()
{
	var numSteps = 0;
	var process;
	while (numSteps < MAX_STEPS && (process = workQueue.shift()))
	{
		if (process.root)
		{
			numSteps = step(numSteps, process);
		}
	}
	if (!process)
	{
		working = false;
		return;
	}
	setTimeout(work, 0);
}


return {
	succeed: succeed,
	fail: fail,
	nativeBinding: nativeBinding,
	andThen: F2(andThen),
	onError: F2(onError),
	receive: receive,

	spawn: spawn,
	kill: kill,
	sleep: sleep,
	send: F2(send),

	rawSpawn: rawSpawn,
	rawSend: rawSend
};

}();
var _elm_lang$core$Platform_Cmd$batch = _elm_lang$core$Native_Platform.batch;
var _elm_lang$core$Platform_Cmd$none = _elm_lang$core$Platform_Cmd$batch(
	{ctor: '[]'});
var _elm_lang$core$Platform_Cmd_ops = _elm_lang$core$Platform_Cmd_ops || {};
_elm_lang$core$Platform_Cmd_ops['!'] = F2(
	function (model, commands) {
		return {
			ctor: '_Tuple2',
			_0: model,
			_1: _elm_lang$core$Platform_Cmd$batch(commands)
		};
	});
var _elm_lang$core$Platform_Cmd$map = _elm_lang$core$Native_Platform.map;
var _elm_lang$core$Platform_Cmd$Cmd = {ctor: 'Cmd'};

var _elm_lang$core$Platform_Sub$batch = _elm_lang$core$Native_Platform.batch;
var _elm_lang$core$Platform_Sub$none = _elm_lang$core$Platform_Sub$batch(
	{ctor: '[]'});
var _elm_lang$core$Platform_Sub$map = _elm_lang$core$Native_Platform.map;
var _elm_lang$core$Platform_Sub$Sub = {ctor: 'Sub'};

var _elm_lang$core$Platform$hack = _elm_lang$core$Native_Scheduler.succeed;
var _elm_lang$core$Platform$sendToSelf = _elm_lang$core$Native_Platform.sendToSelf;
var _elm_lang$core$Platform$sendToApp = _elm_lang$core$Native_Platform.sendToApp;
var _elm_lang$core$Platform$programWithFlags = _elm_lang$core$Native_Platform.programWithFlags;
var _elm_lang$core$Platform$program = _elm_lang$core$Native_Platform.program;
var _elm_lang$core$Platform$Program = {ctor: 'Program'};
var _elm_lang$core$Platform$Task = {ctor: 'Task'};
var _elm_lang$core$Platform$ProcessId = {ctor: 'ProcessId'};
var _elm_lang$core$Platform$Router = {ctor: 'Router'};

var _elm_lang$lazy$Lazy$force = function (_p0) {
	var _p1 = _p0;
	return _p1._0(
		{ctor: '_Tuple0'});
};
var _elm_lang$lazy$Lazy$Lazy = function (a) {
	return {ctor: 'Lazy', _0: a};
};
var _elm_lang$lazy$Lazy$lazy = function (thunk) {
	return _elm_lang$lazy$Lazy$Lazy(
		_elm_lang$lazy$Native_Lazy.memoize(thunk));
};
var _elm_lang$lazy$Lazy$map = F2(
	function (f, a) {
		return _elm_lang$lazy$Lazy$lazy(
			function (_p2) {
				var _p3 = _p2;
				return f(
					_elm_lang$lazy$Lazy$force(a));
			});
	});
var _elm_lang$lazy$Lazy$map2 = F3(
	function (f, a, b) {
		return _elm_lang$lazy$Lazy$lazy(
			function (_p4) {
				var _p5 = _p4;
				return A2(
					f,
					_elm_lang$lazy$Lazy$force(a),
					_elm_lang$lazy$Lazy$force(b));
			});
	});
var _elm_lang$lazy$Lazy$map3 = F4(
	function (f, a, b, c) {
		return _elm_lang$lazy$Lazy$lazy(
			function (_p6) {
				var _p7 = _p6;
				return A3(
					f,
					_elm_lang$lazy$Lazy$force(a),
					_elm_lang$lazy$Lazy$force(b),
					_elm_lang$lazy$Lazy$force(c));
			});
	});
var _elm_lang$lazy$Lazy$map4 = F5(
	function (f, a, b, c, d) {
		return _elm_lang$lazy$Lazy$lazy(
			function (_p8) {
				var _p9 = _p8;
				return A4(
					f,
					_elm_lang$lazy$Lazy$force(a),
					_elm_lang$lazy$Lazy$force(b),
					_elm_lang$lazy$Lazy$force(c),
					_elm_lang$lazy$Lazy$force(d));
			});
	});
var _elm_lang$lazy$Lazy$map5 = F6(
	function (f, a, b, c, d, e) {
		return _elm_lang$lazy$Lazy$lazy(
			function (_p10) {
				var _p11 = _p10;
				return A5(
					f,
					_elm_lang$lazy$Lazy$force(a),
					_elm_lang$lazy$Lazy$force(b),
					_elm_lang$lazy$Lazy$force(c),
					_elm_lang$lazy$Lazy$force(d),
					_elm_lang$lazy$Lazy$force(e));
			});
	});
var _elm_lang$lazy$Lazy$apply = F2(
	function (f, x) {
		return _elm_lang$lazy$Lazy$lazy(
			function (_p12) {
				var _p13 = _p12;
				return A2(
					_elm_lang$lazy$Lazy$force,
					f,
					_elm_lang$lazy$Lazy$force(x));
			});
	});
var _elm_lang$lazy$Lazy$andThen = F2(
	function (callback, a) {
		return _elm_lang$lazy$Lazy$lazy(
			function (_p14) {
				var _p15 = _p14;
				return _elm_lang$lazy$Lazy$force(
					callback(
						_elm_lang$lazy$Lazy$force(a)));
			});
	});

//import Maybe, Native.List //

var _elm_lang$core$Native_Regex = function() {

function escape(str)
{
	return str.replace(/[-\/\\^$*+?.()|[\]{}]/g, '\\$&');
}
function caseInsensitive(re)
{
	return new RegExp(re.source, 'gi');
}
function regex(raw)
{
	return new RegExp(raw, 'g');
}

function contains(re, string)
{
	return string.match(re) !== null;
}

function find(n, re, str)
{
	n = n.ctor === 'All' ? Infinity : n._0;
	var out = [];
	var number = 0;
	var string = str;
	var lastIndex = re.lastIndex;
	var prevLastIndex = -1;
	var result;
	while (number++ < n && (result = re.exec(string)))
	{
		if (prevLastIndex === re.lastIndex) break;
		var i = result.length - 1;
		var subs = new Array(i);
		while (i > 0)
		{
			var submatch = result[i];
			subs[--i] = submatch === undefined
				? _elm_lang$core$Maybe$Nothing
				: _elm_lang$core$Maybe$Just(submatch);
		}
		out.push({
			match: result[0],
			submatches: _elm_lang$core$Native_List.fromArray(subs),
			index: result.index,
			number: number
		});
		prevLastIndex = re.lastIndex;
	}
	re.lastIndex = lastIndex;
	return _elm_lang$core$Native_List.fromArray(out);
}

function replace(n, re, replacer, string)
{
	n = n.ctor === 'All' ? Infinity : n._0;
	var count = 0;
	function jsReplacer(match)
	{
		if (count++ >= n)
		{
			return match;
		}
		var i = arguments.length - 3;
		var submatches = new Array(i);
		while (i > 0)
		{
			var submatch = arguments[i];
			submatches[--i] = submatch === undefined
				? _elm_lang$core$Maybe$Nothing
				: _elm_lang$core$Maybe$Just(submatch);
		}
		return replacer({
			match: match,
			submatches: _elm_lang$core$Native_List.fromArray(submatches),
			index: arguments[arguments.length - 2],
			number: count
		});
	}
	return string.replace(re, jsReplacer);
}

function split(n, re, str)
{
	n = n.ctor === 'All' ? Infinity : n._0;
	if (n === Infinity)
	{
		return _elm_lang$core$Native_List.fromArray(str.split(re));
	}
	var string = str;
	var result;
	var out = [];
	var start = re.lastIndex;
	var restoreLastIndex = re.lastIndex;
	while (n--)
	{
		if (!(result = re.exec(string))) break;
		out.push(string.slice(start, result.index));
		start = re.lastIndex;
	}
	out.push(string.slice(start));
	re.lastIndex = restoreLastIndex;
	return _elm_lang$core$Native_List.fromArray(out);
}

return {
	regex: regex,
	caseInsensitive: caseInsensitive,
	escape: escape,

	contains: F2(contains),
	find: F3(find),
	replace: F4(replace),
	split: F3(split)
};

}();

var _elm_lang$core$Regex$split = _elm_lang$core$Native_Regex.split;
var _elm_lang$core$Regex$replace = _elm_lang$core$Native_Regex.replace;
var _elm_lang$core$Regex$find = _elm_lang$core$Native_Regex.find;
var _elm_lang$core$Regex$contains = _elm_lang$core$Native_Regex.contains;
var _elm_lang$core$Regex$caseInsensitive = _elm_lang$core$Native_Regex.caseInsensitive;
var _elm_lang$core$Regex$regex = _elm_lang$core$Native_Regex.regex;
var _elm_lang$core$Regex$escape = _elm_lang$core$Native_Regex.escape;
var _elm_lang$core$Regex$Match = F4(
	function (a, b, c, d) {
		return {match: a, submatches: b, index: c, number: d};
	});
var _elm_lang$core$Regex$Regex = {ctor: 'Regex'};
var _elm_lang$core$Regex$AtMost = function (a) {
	return {ctor: 'AtMost', _0: a};
};
var _elm_lang$core$Regex$All = {ctor: 'All'};

var _elm_community$parser_combinators$Combine$app = function (p) {
	var _p0 = p;
	if (_p0.ctor === 'Parser') {
		return _p0._0;
	} else {
		return _elm_lang$lazy$Lazy$force(_p0._0);
	}
};
var _elm_community$parser_combinators$Combine$InputStream = F3(
	function (a, b, c) {
		return {data: a, input: b, position: c};
	});
var _elm_community$parser_combinators$Combine$initStream = function (s) {
	return A3(_elm_community$parser_combinators$Combine$InputStream, s, s, 0);
};
var _elm_community$parser_combinators$Combine$runParser = F3(
	function (p, st, s) {
		var _p1 = A3(
			_elm_community$parser_combinators$Combine$app,
			p,
			st,
			_elm_community$parser_combinators$Combine$initStream(s));
		if (_p1._2.ctor === 'Ok') {
			return _elm_lang$core$Result$Ok(
				{ctor: '_Tuple3', _0: _p1._0, _1: _p1._1, _2: _p1._2._0});
		} else {
			return _elm_lang$core$Result$Err(
				{ctor: '_Tuple3', _0: _p1._0, _1: _p1._1, _2: _p1._2._0});
		}
	});
var _elm_community$parser_combinators$Combine$parse = function (p) {
	return A2(
		_elm_community$parser_combinators$Combine$runParser,
		p,
		{ctor: '_Tuple0'});
};
var _elm_community$parser_combinators$Combine$ParseLocation = F3(
	function (a, b, c) {
		return {source: a, line: b, column: c};
	});
var _elm_community$parser_combinators$Combine$currentLocation = function (stream) {
	var find = F3(
		function (position, currentLine, lines) {
			find:
			while (true) {
				var _p2 = lines;
				if (_p2.ctor === '[]') {
					return A3(_elm_community$parser_combinators$Combine$ParseLocation, '', 1, position);
				} else {
					if (_p2._1.ctor === '[]') {
						return A3(_elm_community$parser_combinators$Combine$ParseLocation, _p2._0, currentLine + 1, position);
					} else {
						var _p3 = _p2._0;
						var length = _elm_lang$core$String$length(_p3);
						if (_elm_lang$core$Native_Utils.cmp(position, length) > -1) {
							var _v3 = (position - length) - 1,
								_v4 = currentLine + 1,
								_v5 = _p2._1;
							position = _v3;
							currentLine = _v4;
							lines = _v5;
							continue find;
						} else {
							if (_elm_lang$core$Native_Utils.eq(currentLine, 0)) {
								return A3(_elm_community$parser_combinators$Combine$ParseLocation, _p3, 1, position);
							} else {
								return A3(_elm_community$parser_combinators$Combine$ParseLocation, _p3, currentLine, position - 1);
							}
						}
					}
				}
			}
		});
	return A3(
		find,
		stream.position,
		0,
		A2(_elm_lang$core$String$split, '\n', stream.data));
};
var _elm_community$parser_combinators$Combine$currentSourceLine = function (_p4) {
	return function (_) {
		return _.source;
	}(
		_elm_community$parser_combinators$Combine$currentLocation(_p4));
};
var _elm_community$parser_combinators$Combine$currentLine = function (_p5) {
	return function (_) {
		return _.line;
	}(
		_elm_community$parser_combinators$Combine$currentLocation(_p5));
};
var _elm_community$parser_combinators$Combine$currentColumn = function (_p6) {
	return function (_) {
		return _.column;
	}(
		_elm_community$parser_combinators$Combine$currentLocation(_p6));
};
var _elm_community$parser_combinators$Combine$RecursiveParser = function (a) {
	return {ctor: 'RecursiveParser', _0: a};
};
var _elm_community$parser_combinators$Combine$lazy = function (t) {
	return _elm_community$parser_combinators$Combine$RecursiveParser(
		_elm_lang$lazy$Lazy$lazy(
			function (_p7) {
				var _p8 = _p7;
				return _elm_community$parser_combinators$Combine$app(
					t(
						{ctor: '_Tuple0'}));
			}));
};
var _elm_community$parser_combinators$Combine$Parser = function (a) {
	return {ctor: 'Parser', _0: a};
};
var _elm_community$parser_combinators$Combine$primitive = _elm_community$parser_combinators$Combine$Parser;
var _elm_community$parser_combinators$Combine$bimap = F3(
	function (fok, ferr, p) {
		return _elm_community$parser_combinators$Combine$Parser(
			F2(
				function (state, stream) {
					var _p9 = A3(_elm_community$parser_combinators$Combine$app, p, state, stream);
					if (_p9._2.ctor === 'Ok') {
						return {
							ctor: '_Tuple3',
							_0: _p9._0,
							_1: _p9._1,
							_2: _elm_lang$core$Result$Ok(
								fok(_p9._2._0))
						};
					} else {
						return {
							ctor: '_Tuple3',
							_0: _p9._0,
							_1: _p9._1,
							_2: _elm_lang$core$Result$Err(
								ferr(_p9._2._0))
						};
					}
				}));
	});
var _elm_community$parser_combinators$Combine$map = F2(
	function (f, p) {
		return A3(_elm_community$parser_combinators$Combine$bimap, f, _elm_lang$core$Basics$identity, p);
	});
var _elm_community$parser_combinators$Combine_ops = _elm_community$parser_combinators$Combine_ops || {};
_elm_community$parser_combinators$Combine_ops['<$>'] = _elm_community$parser_combinators$Combine$map;
var _elm_community$parser_combinators$Combine_ops = _elm_community$parser_combinators$Combine_ops || {};
_elm_community$parser_combinators$Combine_ops['<$'] = function (res) {
	return _elm_community$parser_combinators$Combine$map(
		_elm_lang$core$Basics$always(res));
};
var _elm_community$parser_combinators$Combine$skip = function (p) {
	return A2(
		_elm_community$parser_combinators$Combine_ops['<$'],
		{ctor: '_Tuple0'},
		p);
};
var _elm_community$parser_combinators$Combine_ops = _elm_community$parser_combinators$Combine_ops || {};
_elm_community$parser_combinators$Combine_ops['$>'] = _elm_lang$core$Basics$flip(
	F2(
		function (x, y) {
			return A2(_elm_community$parser_combinators$Combine_ops['<$'], x, y);
		}));
var _elm_community$parser_combinators$Combine$mapError = _elm_community$parser_combinators$Combine$bimap(_elm_lang$core$Basics$identity);
var _elm_community$parser_combinators$Combine_ops = _elm_community$parser_combinators$Combine_ops || {};
_elm_community$parser_combinators$Combine_ops['<?>'] = F2(
	function (p, m) {
		return A2(
			_elm_community$parser_combinators$Combine$mapError,
			_elm_lang$core$Basics$always(
				{
					ctor: '::',
					_0: m,
					_1: {ctor: '[]'}
				}),
			p);
	});
var _elm_community$parser_combinators$Combine$withState = function (f) {
	return _elm_community$parser_combinators$Combine$Parser(
		F2(
			function (state, stream) {
				return A3(
					_elm_community$parser_combinators$Combine$app,
					f(state),
					state,
					stream);
			}));
};
var _elm_community$parser_combinators$Combine$withLocation = function (f) {
	return _elm_community$parser_combinators$Combine$Parser(
		F2(
			function (state, stream) {
				return A3(
					_elm_community$parser_combinators$Combine$app,
					f(
						_elm_community$parser_combinators$Combine$currentLocation(stream)),
					state,
					stream);
			}));
};
var _elm_community$parser_combinators$Combine$withLine = function (f) {
	return _elm_community$parser_combinators$Combine$Parser(
		F2(
			function (state, stream) {
				return A3(
					_elm_community$parser_combinators$Combine$app,
					f(
						_elm_community$parser_combinators$Combine$currentLine(stream)),
					state,
					stream);
			}));
};
var _elm_community$parser_combinators$Combine$withColumn = function (f) {
	return _elm_community$parser_combinators$Combine$Parser(
		F2(
			function (state, stream) {
				return A3(
					_elm_community$parser_combinators$Combine$app,
					f(
						_elm_community$parser_combinators$Combine$currentColumn(stream)),
					state,
					stream);
			}));
};
var _elm_community$parser_combinators$Combine$andThen = F2(
	function (f, p) {
		return _elm_community$parser_combinators$Combine$Parser(
			F2(
				function (state, stream) {
					var _p10 = A3(_elm_community$parser_combinators$Combine$app, p, state, stream);
					if (_p10._2.ctor === 'Ok') {
						return A3(
							_elm_community$parser_combinators$Combine$app,
							f(_p10._2._0),
							_p10._0,
							_p10._1);
					} else {
						return {
							ctor: '_Tuple3',
							_0: _p10._0,
							_1: _p10._1,
							_2: _elm_lang$core$Result$Err(_p10._2._0)
						};
					}
				}));
	});
var _elm_community$parser_combinators$Combine_ops = _elm_community$parser_combinators$Combine_ops || {};
_elm_community$parser_combinators$Combine_ops['>>='] = _elm_lang$core$Basics$flip(_elm_community$parser_combinators$Combine$andThen);
var _elm_community$parser_combinators$Combine$andMap = F2(
	function (rp, lp) {
		return A2(
			_elm_community$parser_combinators$Combine_ops['>>='],
			lp,
			A2(_elm_lang$core$Basics$flip, _elm_community$parser_combinators$Combine$map, rp));
	});
var _elm_community$parser_combinators$Combine_ops = _elm_community$parser_combinators$Combine_ops || {};
_elm_community$parser_combinators$Combine_ops['<*>'] = _elm_lang$core$Basics$flip(_elm_community$parser_combinators$Combine$andMap);
var _elm_community$parser_combinators$Combine_ops = _elm_community$parser_combinators$Combine_ops || {};
_elm_community$parser_combinators$Combine_ops['<*'] = F2(
	function (lp, rp) {
		return A2(
			_elm_community$parser_combinators$Combine$andMap,
			rp,
			A2(_elm_community$parser_combinators$Combine$map, _elm_lang$core$Basics$always, lp));
	});
var _elm_community$parser_combinators$Combine_ops = _elm_community$parser_combinators$Combine_ops || {};
_elm_community$parser_combinators$Combine_ops['*>'] = F2(
	function (lp, rp) {
		return A2(
			_elm_community$parser_combinators$Combine$andMap,
			rp,
			A2(
				_elm_community$parser_combinators$Combine$map,
				_elm_lang$core$Basics$flip(_elm_lang$core$Basics$always),
				lp));
	});
var _elm_community$parser_combinators$Combine$between = F3(
	function (lp, rp, p) {
		return A2(
			_elm_community$parser_combinators$Combine_ops['<*'],
			A2(_elm_community$parser_combinators$Combine_ops['*>'], lp, p),
			rp);
	});
var _elm_community$parser_combinators$Combine$sequence = function (parsers) {
	var accumulate = F4(
		function (acc, ps, state, stream) {
			accumulate:
			while (true) {
				var _p11 = ps;
				if (_p11.ctor === '[]') {
					return {
						ctor: '_Tuple3',
						_0: state,
						_1: stream,
						_2: _elm_lang$core$Result$Ok(
							_elm_lang$core$List$reverse(acc))
					};
				} else {
					var _p12 = A3(_elm_community$parser_combinators$Combine$app, _p11._0, state, stream);
					if (_p12._2.ctor === 'Ok') {
						var _v11 = {ctor: '::', _0: _p12._2._0, _1: acc},
							_v12 = _p11._1,
							_v13 = _p12._0,
							_v14 = _p12._1;
						acc = _v11;
						ps = _v12;
						state = _v13;
						stream = _v14;
						continue accumulate;
					} else {
						return {
							ctor: '_Tuple3',
							_0: _p12._0,
							_1: _p12._1,
							_2: _elm_lang$core$Result$Err(_p12._2._0)
						};
					}
				}
			}
		});
	return _elm_community$parser_combinators$Combine$Parser(
		F2(
			function (state, stream) {
				return A4(
					accumulate,
					{ctor: '[]'},
					parsers,
					state,
					stream);
			}));
};
var _elm_community$parser_combinators$Combine$fail = function (m) {
	return _elm_community$parser_combinators$Combine$Parser(
		F2(
			function (state, stream) {
				return {
					ctor: '_Tuple3',
					_0: state,
					_1: stream,
					_2: _elm_lang$core$Result$Err(
						{
							ctor: '::',
							_0: m,
							_1: {ctor: '[]'}
						})
				};
			}));
};
var _elm_community$parser_combinators$Combine$emptyErr = _elm_community$parser_combinators$Combine$Parser(
	F2(
		function (state, stream) {
			return {
				ctor: '_Tuple3',
				_0: state,
				_1: stream,
				_2: _elm_lang$core$Result$Err(
					{ctor: '[]'})
			};
		}));
var _elm_community$parser_combinators$Combine$succeed = function (res) {
	return _elm_community$parser_combinators$Combine$Parser(
		F2(
			function (state, stream) {
				return {
					ctor: '_Tuple3',
					_0: state,
					_1: stream,
					_2: _elm_lang$core$Result$Ok(res)
				};
			}));
};
var _elm_community$parser_combinators$Combine$putState = function (state) {
	return _elm_community$parser_combinators$Combine$Parser(
		F2(
			function (_p13, stream) {
				return A3(
					_elm_community$parser_combinators$Combine$app,
					_elm_community$parser_combinators$Combine$succeed(
						{ctor: '_Tuple0'}),
					state,
					stream);
			}));
};
var _elm_community$parser_combinators$Combine$modifyState = function (f) {
	return _elm_community$parser_combinators$Combine$Parser(
		F2(
			function (state, stream) {
				return A3(
					_elm_community$parser_combinators$Combine$app,
					_elm_community$parser_combinators$Combine$succeed(
						{ctor: '_Tuple0'}),
					f(state),
					stream);
			}));
};
var _elm_community$parser_combinators$Combine$count = F2(
	function (n, p) {
		var accumulate = F2(
			function (x, acc) {
				return (_elm_lang$core$Native_Utils.cmp(x, 0) < 1) ? _elm_community$parser_combinators$Combine$succeed(
					_elm_lang$core$List$reverse(acc)) : A2(
					_elm_community$parser_combinators$Combine$andThen,
					function (res) {
						return A2(
							accumulate,
							x - 1,
							{ctor: '::', _0: res, _1: acc});
					},
					p);
			});
		return A2(
			accumulate,
			n,
			{ctor: '[]'});
	});
var _elm_community$parser_combinators$Combine$string = function (s) {
	return _elm_community$parser_combinators$Combine$Parser(
		F2(
			function (state, stream) {
				if (A2(_elm_lang$core$String$startsWith, s, stream.input)) {
					var len = _elm_lang$core$String$length(s);
					var rem = A2(_elm_lang$core$String$dropLeft, len, stream.input);
					var pos = stream.position + len;
					return {
						ctor: '_Tuple3',
						_0: state,
						_1: _elm_lang$core$Native_Utils.update(
							stream,
							{input: rem, position: pos}),
						_2: _elm_lang$core$Result$Ok(s)
					};
				} else {
					return {
						ctor: '_Tuple3',
						_0: state,
						_1: stream,
						_2: _elm_lang$core$Result$Err(
							{
								ctor: '::',
								_0: A2(
									_elm_lang$core$Basics_ops['++'],
									'expected ',
									_elm_lang$core$Basics$toString(s)),
								_1: {ctor: '[]'}
							})
					};
				}
			}));
};
var _elm_community$parser_combinators$Combine$parens = A2(
	_elm_community$parser_combinators$Combine$between,
	_elm_community$parser_combinators$Combine$string('('),
	_elm_community$parser_combinators$Combine$string(')'));
var _elm_community$parser_combinators$Combine$braces = A2(
	_elm_community$parser_combinators$Combine$between,
	_elm_community$parser_combinators$Combine$string('{'),
	_elm_community$parser_combinators$Combine$string('}'));
var _elm_community$parser_combinators$Combine$brackets = A2(
	_elm_community$parser_combinators$Combine$between,
	_elm_community$parser_combinators$Combine$string('['),
	_elm_community$parser_combinators$Combine$string(']'));
var _elm_community$parser_combinators$Combine$regex = function (pat) {
	var pattern = A2(_elm_lang$core$String$startsWith, '^', pat) ? pat : A2(_elm_lang$core$Basics_ops['++'], '^', pat);
	return _elm_community$parser_combinators$Combine$Parser(
		F2(
			function (state, stream) {
				var _p14 = A3(
					_elm_lang$core$Regex$find,
					_elm_lang$core$Regex$AtMost(1),
					_elm_lang$core$Regex$regex(pattern),
					stream.input);
				if ((_p14.ctor === '::') && (_p14._1.ctor === '[]')) {
					var _p15 = _p14._0;
					var len = _elm_lang$core$String$length(_p15.match);
					var rem = A2(_elm_lang$core$String$dropLeft, len, stream.input);
					var pos = stream.position + len;
					return {
						ctor: '_Tuple3',
						_0: state,
						_1: _elm_lang$core$Native_Utils.update(
							stream,
							{input: rem, position: pos}),
						_2: _elm_lang$core$Result$Ok(_p15.match)
					};
				} else {
					return {
						ctor: '_Tuple3',
						_0: state,
						_1: stream,
						_2: _elm_lang$core$Result$Err(
							{
								ctor: '::',
								_0: A2(
									_elm_lang$core$Basics_ops['++'],
									'expected input matching Regexp /',
									A2(_elm_lang$core$Basics_ops['++'], pattern, '/')),
								_1: {ctor: '[]'}
							})
					};
				}
			}));
};
var _elm_community$parser_combinators$Combine$whitespace = A2(
	_elm_community$parser_combinators$Combine_ops['<?>'],
	_elm_community$parser_combinators$Combine$regex('[ \t\r\n]*'),
	'whitespace');
var _elm_community$parser_combinators$Combine$whitespace1 = A2(
	_elm_community$parser_combinators$Combine_ops['<?>'],
	_elm_community$parser_combinators$Combine$regex('[ \t\r\n]+'),
	'whitespace');
var _elm_community$parser_combinators$Combine$while = function (pred) {
	var accumulate = F3(
		function (acc, state, stream) {
			accumulate:
			while (true) {
				var _p16 = _elm_lang$core$String$uncons(stream.input);
				if (_p16.ctor === 'Just') {
					var _p17 = _p16._0._0;
					if (pred(_p17)) {
						var pos = stream.position + 1;
						var c = A2(_elm_lang$core$String$cons, _p17, '');
						var _v17 = A2(_elm_lang$core$Basics_ops['++'], acc, c),
							_v18 = state,
							_v19 = _elm_lang$core$Native_Utils.update(
							stream,
							{input: _p16._0._1, position: pos});
						acc = _v17;
						state = _v18;
						stream = _v19;
						continue accumulate;
					} else {
						return {ctor: '_Tuple3', _0: state, _1: stream, _2: acc};
					}
				} else {
					return {ctor: '_Tuple3', _0: state, _1: stream, _2: acc};
				}
			}
		});
	return _elm_community$parser_combinators$Combine$Parser(
		F2(
			function (state, stream) {
				var _p18 = A3(accumulate, '', state, stream);
				var rstate = _p18._0;
				var rstream = _p18._1;
				var res = _p18._2;
				return {
					ctor: '_Tuple3',
					_0: rstate,
					_1: rstream,
					_2: _elm_lang$core$Result$Ok(res)
				};
			}));
};
var _elm_community$parser_combinators$Combine$end = _elm_community$parser_combinators$Combine$Parser(
	F2(
		function (state, stream) {
			return _elm_lang$core$Native_Utils.eq(stream.input, '') ? {
				ctor: '_Tuple3',
				_0: state,
				_1: stream,
				_2: _elm_lang$core$Result$Ok(
					{ctor: '_Tuple0'})
			} : {
				ctor: '_Tuple3',
				_0: state,
				_1: stream,
				_2: _elm_lang$core$Result$Err(
					{
						ctor: '::',
						_0: 'expected end of input',
						_1: {ctor: '[]'}
					})
			};
		}));
var _elm_community$parser_combinators$Combine$lookAhead = function (p) {
	return _elm_community$parser_combinators$Combine$Parser(
		F2(
			function (state, stream) {
				var _p19 = A3(_elm_community$parser_combinators$Combine$app, p, state, stream);
				if ((_p19.ctor === '_Tuple3') && (_p19._2.ctor === 'Ok')) {
					return {
						ctor: '_Tuple3',
						_0: _p19._0,
						_1: stream,
						_2: _elm_lang$core$Result$Ok(_p19._2._0)
					};
				} else {
					return _p19;
				}
			}));
};
var _elm_community$parser_combinators$Combine$or = F2(
	function (lp, rp) {
		return _elm_community$parser_combinators$Combine$Parser(
			F2(
				function (state, stream) {
					var _p20 = A3(_elm_community$parser_combinators$Combine$app, lp, state, stream);
					if (_p20._2.ctor === 'Ok') {
						return _p20;
					} else {
						var _p21 = A3(_elm_community$parser_combinators$Combine$app, rp, state, stream);
						if (_p21._2.ctor === 'Ok') {
							return _p21;
						} else {
							return {
								ctor: '_Tuple3',
								_0: state,
								_1: stream,
								_2: _elm_lang$core$Result$Err(
									A2(_elm_lang$core$Basics_ops['++'], _p20._2._0, _p21._2._0))
							};
						}
					}
				}));
	});
var _elm_community$parser_combinators$Combine$choice = function (xs) {
	return A3(_elm_lang$core$List$foldr, _elm_community$parser_combinators$Combine$or, _elm_community$parser_combinators$Combine$emptyErr, xs);
};
var _elm_community$parser_combinators$Combine_ops = _elm_community$parser_combinators$Combine_ops || {};
_elm_community$parser_combinators$Combine_ops['<|>'] = _elm_community$parser_combinators$Combine$or;
var _elm_community$parser_combinators$Combine$optional = F2(
	function (res, p) {
		return A2(
			_elm_community$parser_combinators$Combine_ops['<|>'],
			p,
			_elm_community$parser_combinators$Combine$succeed(res));
	});
var _elm_community$parser_combinators$Combine$chainl = F2(
	function (op, p) {
		var accumulate = function (x) {
			return A2(
				_elm_community$parser_combinators$Combine_ops['<|>'],
				A2(
					_elm_community$parser_combinators$Combine$andThen,
					function (f) {
						return A2(
							_elm_community$parser_combinators$Combine$andThen,
							function (y) {
								return accumulate(
									A2(f, x, y));
							},
							p);
					},
					op),
				_elm_community$parser_combinators$Combine$succeed(x));
		};
		return A2(_elm_community$parser_combinators$Combine$andThen, accumulate, p);
	});
var _elm_community$parser_combinators$Combine$chainr = F2(
	function (op, p) {
		var accumulate = function (x) {
			return A2(
				_elm_community$parser_combinators$Combine_ops['<|>'],
				A2(
					_elm_community$parser_combinators$Combine$andThen,
					function (f) {
						return A2(
							_elm_community$parser_combinators$Combine$andThen,
							function (y) {
								return _elm_community$parser_combinators$Combine$succeed(
									A2(f, x, y));
							},
							A2(_elm_community$parser_combinators$Combine$andThen, accumulate, p));
					},
					op),
				_elm_community$parser_combinators$Combine$succeed(x));
		};
		return A2(_elm_community$parser_combinators$Combine$andThen, accumulate, p);
	});
var _elm_community$parser_combinators$Combine$maybe = function (p) {
	return _elm_community$parser_combinators$Combine$Parser(
		F2(
			function (state, stream) {
				var _p22 = A3(_elm_community$parser_combinators$Combine$app, p, state, stream);
				if ((_p22.ctor === '_Tuple3') && (_p22._2.ctor === 'Ok')) {
					return {
						ctor: '_Tuple3',
						_0: _p22._0,
						_1: _p22._1,
						_2: _elm_lang$core$Result$Ok(
							_elm_lang$core$Maybe$Just(_p22._2._0))
					};
				} else {
					return {
						ctor: '_Tuple3',
						_0: state,
						_1: stream,
						_2: _elm_lang$core$Result$Ok(_elm_lang$core$Maybe$Nothing)
					};
				}
			}));
};
var _elm_community$parser_combinators$Combine$many = function (p) {
	var accumulate = F3(
		function (acc, state, stream) {
			accumulate:
			while (true) {
				var _p23 = A3(_elm_community$parser_combinators$Combine$app, p, state, stream);
				if ((_p23.ctor === '_Tuple3') && (_p23._2.ctor === 'Ok')) {
					var _p25 = _p23._1;
					var _p24 = _p23._0;
					if (_elm_lang$core$Native_Utils.eq(stream, _p25)) {
						return {
							ctor: '_Tuple3',
							_0: _p24,
							_1: _p25,
							_2: _elm_lang$core$List$reverse(acc)
						};
					} else {
						var _v25 = {ctor: '::', _0: _p23._2._0, _1: acc},
							_v26 = _p24,
							_v27 = _p25;
						acc = _v25;
						state = _v26;
						stream = _v27;
						continue accumulate;
					}
				} else {
					return {
						ctor: '_Tuple3',
						_0: state,
						_1: stream,
						_2: _elm_lang$core$List$reverse(acc)
					};
				}
			}
		});
	return _elm_community$parser_combinators$Combine$Parser(
		F2(
			function (state, stream) {
				var _p26 = A3(
					accumulate,
					{ctor: '[]'},
					state,
					stream);
				var rstate = _p26._0;
				var rstream = _p26._1;
				var res = _p26._2;
				return {
					ctor: '_Tuple3',
					_0: rstate,
					_1: rstream,
					_2: _elm_lang$core$Result$Ok(res)
				};
			}));
};
var _elm_community$parser_combinators$Combine$many1 = function (p) {
	return A2(
		_elm_community$parser_combinators$Combine_ops['<*>'],
		A2(
			_elm_community$parser_combinators$Combine_ops['<$>'],
			F2(
				function (x, y) {
					return {ctor: '::', _0: x, _1: y};
				}),
			p),
		_elm_community$parser_combinators$Combine$many(p));
};
var _elm_community$parser_combinators$Combine$skipMany1 = function (p) {
	return A2(
		_elm_community$parser_combinators$Combine_ops['<$'],
		{ctor: '_Tuple0'},
		_elm_community$parser_combinators$Combine$many1(
			_elm_community$parser_combinators$Combine$skip(p)));
};
var _elm_community$parser_combinators$Combine$sepBy1 = F2(
	function (sep, p) {
		return A2(
			_elm_community$parser_combinators$Combine_ops['<*>'],
			A2(
				_elm_community$parser_combinators$Combine_ops['<$>'],
				F2(
					function (x, y) {
						return {ctor: '::', _0: x, _1: y};
					}),
				p),
			_elm_community$parser_combinators$Combine$many(
				A2(_elm_community$parser_combinators$Combine_ops['*>'], sep, p)));
	});
var _elm_community$parser_combinators$Combine$sepBy = F2(
	function (sep, p) {
		return A2(
			_elm_community$parser_combinators$Combine_ops['<|>'],
			A2(_elm_community$parser_combinators$Combine$sepBy1, sep, p),
			_elm_community$parser_combinators$Combine$succeed(
				{ctor: '[]'}));
	});
var _elm_community$parser_combinators$Combine$sepEndBy1 = F2(
	function (sep, p) {
		return A2(
			_elm_community$parser_combinators$Combine_ops['<*'],
			A2(_elm_community$parser_combinators$Combine$sepBy1, sep, p),
			_elm_community$parser_combinators$Combine$maybe(sep));
	});
var _elm_community$parser_combinators$Combine$sepEndBy = F2(
	function (sep, p) {
		return A2(
			_elm_community$parser_combinators$Combine_ops['<|>'],
			A2(_elm_community$parser_combinators$Combine$sepEndBy1, sep, p),
			_elm_community$parser_combinators$Combine$succeed(
				{ctor: '[]'}));
	});
var _elm_community$parser_combinators$Combine$skipMany = function (p) {
	return A2(
		_elm_community$parser_combinators$Combine_ops['<$'],
		{ctor: '_Tuple0'},
		_elm_community$parser_combinators$Combine$many(
			_elm_community$parser_combinators$Combine$skip(p)));
};
var _elm_community$parser_combinators$Combine$manyTill = F2(
	function (p, end) {
		var accumulate = F3(
			function (acc, state, stream) {
				accumulate:
				while (true) {
					var _p27 = A3(_elm_community$parser_combinators$Combine$app, end, state, stream);
					if (_p27._2.ctor === 'Ok') {
						return {
							ctor: '_Tuple3',
							_0: _p27._0,
							_1: _p27._1,
							_2: _elm_lang$core$Result$Ok(
								_elm_lang$core$List$reverse(acc))
						};
					} else {
						var _p28 = A3(_elm_community$parser_combinators$Combine$app, p, state, stream);
						if ((_p28.ctor === '_Tuple3') && (_p28._2.ctor === 'Ok')) {
							var _v30 = {ctor: '::', _0: _p28._2._0, _1: acc},
								_v31 = _p28._0,
								_v32 = _p28._1;
							acc = _v30;
							state = _v31;
							stream = _v32;
							continue accumulate;
						} else {
							return {
								ctor: '_Tuple3',
								_0: _p27._0,
								_1: _p27._1,
								_2: _elm_lang$core$Result$Err(_p27._2._0)
							};
						}
					}
				}
			});
		return _elm_community$parser_combinators$Combine$Parser(
			accumulate(
				{ctor: '[]'}));
	});

var _elm_community$parser_combinators$Combine_Char$crlf = A2(
	_elm_community$parser_combinators$Combine_ops['<$'],
	_elm_lang$core$Native_Utils.chr('\n'),
	A2(
		_elm_community$parser_combinators$Combine_ops['<?>'],
		_elm_community$parser_combinators$Combine$regex('\r\n'),
		'expected crlf'));
var _elm_community$parser_combinators$Combine_Char$satisfy = function (pred) {
	return _elm_community$parser_combinators$Combine$primitive(
		F2(
			function (state, stream) {
				var message = 'could not satisfy predicate';
				var _p0 = _elm_lang$core$String$uncons(stream.input);
				if (_p0.ctor === 'Just') {
					var _p1 = _p0._0._0;
					return pred(_p1) ? {
						ctor: '_Tuple3',
						_0: state,
						_1: _elm_lang$core$Native_Utils.update(
							stream,
							{input: _p0._0._1, position: stream.position + 1}),
						_2: _elm_lang$core$Result$Ok(_p1)
					} : {
						ctor: '_Tuple3',
						_0: state,
						_1: stream,
						_2: _elm_lang$core$Result$Err(
							{
								ctor: '::',
								_0: message,
								_1: {ctor: '[]'}
							})
					};
				} else {
					return {
						ctor: '_Tuple3',
						_0: state,
						_1: stream,
						_2: _elm_lang$core$Result$Err(
							{
								ctor: '::',
								_0: message,
								_1: {ctor: '[]'}
							})
					};
				}
			}));
};
var _elm_community$parser_combinators$Combine_Char$char = function (c) {
	return A2(
		_elm_community$parser_combinators$Combine_ops['<?>'],
		_elm_community$parser_combinators$Combine_Char$satisfy(
			F2(
				function (x, y) {
					return _elm_lang$core$Native_Utils.eq(x, y);
				})(c)),
		A2(
			_elm_lang$core$Basics_ops['++'],
			'expected ',
			_elm_lang$core$Basics$toString(c)));
};
var _elm_community$parser_combinators$Combine_Char$anyChar = A2(
	_elm_community$parser_combinators$Combine_ops['<?>'],
	_elm_community$parser_combinators$Combine_Char$satisfy(
		_elm_lang$core$Basics$always(true)),
	'expected any character');
var _elm_community$parser_combinators$Combine_Char$oneOf = function (cs) {
	return A2(
		_elm_community$parser_combinators$Combine_ops['<?>'],
		_elm_community$parser_combinators$Combine_Char$satisfy(
			A2(_elm_lang$core$Basics$flip, _elm_lang$core$List$member, cs)),
		A2(
			_elm_lang$core$Basics_ops['++'],
			'expected one of ',
			_elm_lang$core$Basics$toString(cs)));
};
var _elm_community$parser_combinators$Combine_Char$noneOf = function (cs) {
	return A2(
		_elm_community$parser_combinators$Combine_ops['<?>'],
		_elm_community$parser_combinators$Combine_Char$satisfy(
			function (_p2) {
				return !A3(_elm_lang$core$Basics$flip, _elm_lang$core$List$member, cs, _p2);
			}),
		A2(
			_elm_lang$core$Basics_ops['++'],
			'expected none of ',
			_elm_lang$core$Basics$toString(cs)));
};
var _elm_community$parser_combinators$Combine_Char$space = A2(
	_elm_community$parser_combinators$Combine_ops['<?>'],
	_elm_community$parser_combinators$Combine_Char$satisfy(
		F2(
			function (x, y) {
				return _elm_lang$core$Native_Utils.eq(x, y);
			})(
			_elm_lang$core$Native_Utils.chr(' '))),
	'expected space');
var _elm_community$parser_combinators$Combine_Char$tab = A2(
	_elm_community$parser_combinators$Combine_ops['<?>'],
	_elm_community$parser_combinators$Combine_Char$satisfy(
		F2(
			function (x, y) {
				return _elm_lang$core$Native_Utils.eq(x, y);
			})(
			_elm_lang$core$Native_Utils.chr('\t'))),
	'expected tab');
var _elm_community$parser_combinators$Combine_Char$newline = A2(
	_elm_community$parser_combinators$Combine_ops['<?>'],
	_elm_community$parser_combinators$Combine_Char$satisfy(
		F2(
			function (x, y) {
				return _elm_lang$core$Native_Utils.eq(x, y);
			})(
			_elm_lang$core$Native_Utils.chr('\n'))),
	'expected newline');
var _elm_community$parser_combinators$Combine_Char$eol = A2(_elm_community$parser_combinators$Combine_ops['<|>'], _elm_community$parser_combinators$Combine_Char$newline, _elm_community$parser_combinators$Combine_Char$crlf);
var _elm_community$parser_combinators$Combine_Char$lower = A2(
	_elm_community$parser_combinators$Combine_ops['<?>'],
	_elm_community$parser_combinators$Combine_Char$satisfy(_elm_lang$core$Char$isLower),
	'expected a lowercase character');
var _elm_community$parser_combinators$Combine_Char$upper = A2(
	_elm_community$parser_combinators$Combine_ops['<?>'],
	_elm_community$parser_combinators$Combine_Char$satisfy(_elm_lang$core$Char$isUpper),
	'expected an uppercase character');
var _elm_community$parser_combinators$Combine_Char$digit = A2(
	_elm_community$parser_combinators$Combine_ops['<?>'],
	_elm_community$parser_combinators$Combine_Char$satisfy(_elm_lang$core$Char$isDigit),
	'expected a digit');
var _elm_community$parser_combinators$Combine_Char$octDigit = A2(
	_elm_community$parser_combinators$Combine_ops['<?>'],
	_elm_community$parser_combinators$Combine_Char$satisfy(_elm_lang$core$Char$isOctDigit),
	'expected an octal digit');
var _elm_community$parser_combinators$Combine_Char$hexDigit = A2(
	_elm_community$parser_combinators$Combine_ops['<?>'],
	_elm_community$parser_combinators$Combine_Char$satisfy(_elm_lang$core$Char$isHexDigit),
	'expected a hexadecimal digit');

var _elm_community$parser_combinators$Combine_Num$digit = function () {
	var toDigit = function (c) {
		return _elm_lang$core$Char$toCode(c) - _elm_lang$core$Char$toCode(
			_elm_lang$core$Native_Utils.chr('0'));
	};
	return A2(
		_elm_community$parser_combinators$Combine_ops['<$>'],
		toDigit,
		A2(_elm_community$parser_combinators$Combine_ops['<?>'], _elm_community$parser_combinators$Combine_Char$digit, 'expected a digit'));
}();
var _elm_community$parser_combinators$Combine_Num$sign = A2(
	_elm_community$parser_combinators$Combine$optional,
	1,
	_elm_community$parser_combinators$Combine$choice(
		{
			ctor: '::',
			_0: A2(
				_elm_community$parser_combinators$Combine_ops['<$'],
				1,
				_elm_community$parser_combinators$Combine$string('+')),
			_1: {
				ctor: '::',
				_0: A2(
					_elm_community$parser_combinators$Combine_ops['<$'],
					-1,
					_elm_community$parser_combinators$Combine$string('-')),
				_1: {ctor: '[]'}
			}
		}));
var _elm_community$parser_combinators$Combine_Num$unwrap = F2(
	function (f, s) {
		var _p0 = f(s);
		if (_p0.ctor === 'Ok') {
			return _p0._0;
		} else {
			return _elm_lang$core$Native_Utils.crashCase(
				'Combine.Num',
				{
					start: {line: 23, column: 5},
					end: {line: 28, column: 83}
				},
				_p0)(
				A2(
					_elm_lang$core$Basics_ops['++'],
					'impossible state in Combine.Num.unwrap: ',
					_elm_lang$core$Basics$toString(_p0._0)));
		}
	});
var _elm_community$parser_combinators$Combine_Num$toInt = _elm_community$parser_combinators$Combine_Num$unwrap(_elm_lang$core$String$toInt);
var _elm_community$parser_combinators$Combine_Num$int = A2(
	_elm_community$parser_combinators$Combine_ops['<*>'],
	A2(
		_elm_community$parser_combinators$Combine_ops['<$>'],
		F2(
			function (x, y) {
				return x * y;
			}),
		_elm_community$parser_combinators$Combine_Num$sign),
	A2(
		_elm_community$parser_combinators$Combine_ops['<?>'],
		A2(
			_elm_community$parser_combinators$Combine_ops['<$>'],
			_elm_community$parser_combinators$Combine_Num$toInt,
			_elm_community$parser_combinators$Combine$regex('(0|[1-9][0-9]*)')),
		'expected an integer'));
var _elm_community$parser_combinators$Combine_Num$toFloat = _elm_community$parser_combinators$Combine_Num$unwrap(_elm_lang$core$String$toFloat);
var _elm_community$parser_combinators$Combine_Num$float = A2(
	_elm_community$parser_combinators$Combine_ops['<*>'],
	A2(
		_elm_community$parser_combinators$Combine_ops['<$>'],
		function (_p2) {
			return F2(
				function (x, y) {
					return x * y;
				})(
				_elm_lang$core$Basics$toFloat(_p2));
		},
		_elm_community$parser_combinators$Combine_Num$sign),
	A2(
		_elm_community$parser_combinators$Combine_ops['<?>'],
		A2(
			_elm_community$parser_combinators$Combine_ops['<$>'],
			_elm_community$parser_combinators$Combine_Num$toFloat,
			_elm_community$parser_combinators$Combine$regex('(0|[1-9][0-9]*)(\\.[0-9]+)')),
		'expected a float'));

var _elm_community$elm_time$Time_Internal$digitsInRange = F3(
	function (digitsToParse, lo, hi) {
		var failure = _elm_community$parser_combinators$Combine$fail(
			A2(
				_elm_lang$core$Basics_ops['++'],
				'expected ',
				A2(
					_elm_lang$core$Basics_ops['++'],
					_elm_lang$core$Basics$toString(digitsToParse),
					A2(
						_elm_lang$core$Basics_ops['++'],
						' digits in the range [',
						A2(
							_elm_lang$core$Basics_ops['++'],
							_elm_lang$core$Basics$toString(lo),
							A2(
								_elm_lang$core$Basics_ops['++'],
								', ',
								A2(
									_elm_lang$core$Basics_ops['++'],
									_elm_lang$core$Basics$toString(hi),
									']')))))));
		return A2(
			_elm_community$parser_combinators$Combine$andThen,
			function (digits) {
				var _p0 = _elm_lang$core$String$toInt(digits);
				if (_p0.ctor === 'Ok') {
					var _p1 = _p0._0;
					return ((_elm_lang$core$Native_Utils.cmp(_p1, lo) > -1) && (_elm_lang$core$Native_Utils.cmp(_p1, hi) < 1)) ? _elm_community$parser_combinators$Combine$succeed(_p1) : failure;
				} else {
					return failure;
				}
			},
			_elm_community$parser_combinators$Combine$regex(
				A2(_elm_lang$core$String$repeat, digitsToParse, '\\d')));
	});
var _elm_community$elm_time$Time_Internal$paddedInt = A2(
	_elm_community$parser_combinators$Combine_ops['*>'],
	A2(
		_elm_community$parser_combinators$Combine$optional,
		'',
		_elm_community$parser_combinators$Combine$string('0')),
	_elm_community$parser_combinators$Combine_Num$int);
var _elm_community$elm_time$Time_Internal$intRange = F2(
	function (lo, hi) {
		var validate = function (n) {
			return ((_elm_lang$core$Native_Utils.cmp(n, lo) > -1) && (_elm_lang$core$Native_Utils.cmp(n, hi) < 1)) ? _elm_community$parser_combinators$Combine$succeed(n) : _elm_community$parser_combinators$Combine$fail(
				A2(
					_elm_lang$core$Basics_ops['++'],
					'expected an integer in the range [',
					A2(
						_elm_lang$core$Basics_ops['++'],
						_elm_lang$core$Basics$toString(lo),
						A2(
							_elm_lang$core$Basics_ops['++'],
							', ',
							A2(
								_elm_lang$core$Basics_ops['++'],
								_elm_lang$core$Basics$toString(hi),
								']')))));
		};
		return A2(_elm_community$parser_combinators$Combine_ops['>>='], _elm_community$elm_time$Time_Internal$paddedInt, validate);
	});
var _elm_community$elm_time$Time_Internal$secondMs = 1000;
var _elm_community$elm_time$Time_Internal$minuteMs = 60000;
var _elm_community$elm_time$Time_Internal$hourMs = 3600000;
var _elm_community$elm_time$Time_Internal$dayMs = 86400000;
var _elm_community$elm_time$Time_Internal$padded3 = function (n) {
	return A3(
		_elm_lang$core$String$padLeft,
		3,
		_elm_lang$core$Native_Utils.chr('0'),
		_elm_lang$core$Basics$toString(n));
};
var _elm_community$elm_time$Time_Internal$padded = function (n) {
	return (_elm_lang$core$Native_Utils.cmp(n, 10) < 0) ? A2(
		_elm_lang$core$Basics_ops['++'],
		'0',
		_elm_lang$core$Basics$toString(n)) : _elm_lang$core$Basics$toString(n);
};
var _elm_community$elm_time$Time_Internal$zero = {year: 0, month: 1, day: 1, hour: 0, minute: 0, second: 0, millisecond: 0};
var _elm_community$elm_time$Time_Internal$offsetFromTimeData = function (_p2) {
	var _p3 = _p2;
	return (((A3(_elm_lang$core$Basics$clamp, 0, 23, _p3.hour) * _elm_community$elm_time$Time_Internal$hourMs) + (A3(_elm_lang$core$Basics$clamp, 0, 59, _p3.minute) * _elm_community$elm_time$Time_Internal$minuteMs)) + (A3(_elm_lang$core$Basics$clamp, 0, 59, _p3.second) * _elm_community$elm_time$Time_Internal$secondMs)) + A3(_elm_lang$core$Basics$clamp, 0, 999, _p3.millisecond);
};
var _elm_community$elm_time$Time_Internal$DateTimeData = F7(
	function (a, b, c, d, e, f, g) {
		return {year: a, month: b, day: c, hour: d, minute: e, second: f, millisecond: g};
	});

var _elm_community$elm_time$Time_Date$clampDay = function (day) {
	return A3(_elm_lang$core$Basics$clamp, 1, 31, day);
};
var _elm_community$elm_time$Time_Date$clampMonth = function (month) {
	return A3(_elm_lang$core$Basics$clamp, 1, 12, month);
};
var _elm_community$elm_time$Time_Date$daysFromYear = function (y) {
	return (_elm_lang$core$Native_Utils.cmp(y, 0) > 0) ? ((((366 + ((y - 1) * 365)) + (((y - 1) / 4) | 0)) - (((y - 1) / 100) | 0)) + (((y - 1) / 400) | 0)) : ((_elm_lang$core$Native_Utils.cmp(y, 0) < 0) ? ((((y * 365) + ((y / 4) | 0)) - ((y / 100) | 0)) + ((y / 400) | 0)) : 0);
};
var _elm_community$elm_time$Time_Date$yearFromDays = function (ds) {
	var y = (ds / 365) | 0;
	var d = _elm_community$elm_time$Time_Date$daysFromYear(y);
	return (_elm_lang$core$Native_Utils.cmp(ds, d) < 1) ? (y - 1) : y;
};
var _elm_community$elm_time$Time_Date$isLeapYear = function (y) {
	return _elm_lang$core$Native_Utils.eq(
		A2(_elm_lang$core$Basics_ops['%'], y, 400),
		0) || ((!_elm_lang$core$Native_Utils.eq(
		A2(_elm_lang$core$Basics_ops['%'], y, 100),
		0)) && _elm_lang$core$Native_Utils.eq(
		A2(_elm_lang$core$Basics_ops['%'], y, 4),
		0));
};
var _elm_community$elm_time$Time_Date$unsafeDaysInMonth = F2(
	function (y, m) {
		return _elm_lang$core$Native_Utils.eq(m, 1) ? 31 : ((_elm_lang$core$Native_Utils.eq(m, 2) && _elm_community$elm_time$Time_Date$isLeapYear(y)) ? 29 : (_elm_lang$core$Native_Utils.eq(m, 2) ? 28 : (_elm_lang$core$Native_Utils.eq(m, 3) ? 31 : (_elm_lang$core$Native_Utils.eq(m, 4) ? 30 : (_elm_lang$core$Native_Utils.eq(m, 5) ? 31 : (_elm_lang$core$Native_Utils.eq(m, 6) ? 30 : (_elm_lang$core$Native_Utils.eq(m, 7) ? 31 : (_elm_lang$core$Native_Utils.eq(m, 8) ? 31 : (_elm_lang$core$Native_Utils.eq(m, 9) ? 30 : (_elm_lang$core$Native_Utils.eq(m, 10) ? 31 : (_elm_lang$core$Native_Utils.eq(m, 11) ? 30 : (_elm_lang$core$Native_Utils.eq(m, 12) ? 31 : _elm_lang$core$Native_Utils.crash(
			'Time.Date',
			{
				start: {line: 343, column: 9},
				end: {line: 343, column: 20}
			})(
			A2(
				_elm_lang$core$Basics_ops['++'],
				'invalid call to unsafeDaysInMonth: year=',
				A2(
					_elm_lang$core$Basics_ops['++'],
					_elm_lang$core$Basics$toString(y),
					A2(
						_elm_lang$core$Basics_ops['++'],
						' month=',
						_elm_lang$core$Basics$toString(m)))))))))))))))));
	});
var _elm_community$elm_time$Time_Date$daysInMonth = F2(
	function (y, m) {
		return ((_elm_lang$core$Native_Utils.cmp(m, 1) > -1) && (_elm_lang$core$Native_Utils.cmp(m, 12) < 1)) ? _elm_lang$core$Maybe$Just(
			A2(_elm_community$elm_time$Time_Date$unsafeDaysInMonth, y, m)) : _elm_lang$core$Maybe$Nothing;
	});
var _elm_community$elm_time$Time_Date$daysFromYearMonth = F2(
	function (year, month) {
		var go = F3(
			function (year, month, acc) {
				go:
				while (true) {
					if (_elm_lang$core$Native_Utils.eq(month, 0)) {
						return acc;
					} else {
						var _v0 = year,
							_v1 = month - 1,
							_v2 = acc + A2(_elm_community$elm_time$Time_Date$unsafeDaysInMonth, year, month);
						year = _v0;
						month = _v1;
						acc = _v2;
						continue go;
					}
				}
			});
		return A3(go, year, month - 1, 0);
	});
var _elm_community$elm_time$Time_Date$daysFromYearMonthDay = F3(
	function (year, month, day) {
		var dds = day - 1;
		var mds = A2(_elm_community$elm_time$Time_Date$daysFromYearMonth, year, month);
		var yds = _elm_community$elm_time$Time_Date$daysFromYear(year);
		return (yds + mds) + dds;
	});
var _elm_community$elm_time$Time_Date$isValidDate = F3(
	function (year, month, day) {
		return A2(
			_elm_lang$core$Maybe$withDefault,
			false,
			A2(
				_elm_lang$core$Maybe$map,
				function (days) {
					return (_elm_lang$core$Native_Utils.cmp(day, 1) > -1) && (_elm_lang$core$Native_Utils.cmp(day, days) < 1);
				},
				A2(_elm_community$elm_time$Time_Date$daysInMonth, year, month)));
	});
var _elm_community$elm_time$Time_Date$toTuple = function (_p0) {
	var _p1 = _p0;
	return {ctor: '_Tuple3', _0: _p1._0.year, _1: _p1._0.month, _2: _p1._0.day};
};
var _elm_community$elm_time$Time_Date$delta = F2(
	function (_p3, _p2) {
		var _p4 = _p3;
		var _p7 = _p4._0;
		var _p5 = _p2;
		var _p6 = _p5._0;
		return {
			years: _p7.year - _p6.year,
			months: ((_elm_lang$core$Basics$abs(_p7.year) * 12) + _p7.month) - ((_elm_lang$core$Basics$abs(_p6.year) * 12) + _p6.month),
			days: A3(_elm_community$elm_time$Time_Date$daysFromYearMonthDay, _p7.year, _p7.month, _p7.day) - A3(_elm_community$elm_time$Time_Date$daysFromYearMonthDay, _p6.year, _p6.month, _p6.day)
		};
	});
var _elm_community$elm_time$Time_Date$compare = F2(
	function (d1, d2) {
		return A2(
			_elm_lang$core$Basics$compare,
			_elm_community$elm_time$Time_Date$toTuple(d1),
			_elm_community$elm_time$Time_Date$toTuple(d2));
	});
var _elm_community$elm_time$Time_Date$day = function (_p8) {
	var _p9 = _p8;
	return _p9._0.day;
};
var _elm_community$elm_time$Time_Date$month = function (_p10) {
	var _p11 = _p10;
	return _p11._0.month;
};
var _elm_community$elm_time$Time_Date$year = function (_p12) {
	var _p13 = _p12;
	return _p13._0.year;
};
var _elm_community$elm_time$Time_Date$toISO8601 = function (d) {
	return A2(
		_elm_lang$core$Basics_ops['++'],
		_elm_lang$core$Basics$toString(
			_elm_community$elm_time$Time_Date$year(d)),
		A2(
			_elm_lang$core$Basics_ops['++'],
			'-',
			A2(
				_elm_lang$core$Basics_ops['++'],
				_elm_community$elm_time$Time_Internal$padded(
					_elm_community$elm_time$Time_Date$month(d)),
				A2(
					_elm_lang$core$Basics_ops['++'],
					'-',
					_elm_community$elm_time$Time_Internal$padded(
						_elm_community$elm_time$Time_Date$day(d))))));
};
var _elm_community$elm_time$Time_Date$DateDelta = F3(
	function (a, b, c) {
		return {years: a, months: b, days: c};
	});
var _elm_community$elm_time$Time_Date$Date = function (a) {
	return {ctor: 'Date', _0: a};
};
var _elm_community$elm_time$Time_Date$firstValid = F3(
	function (year, month, day) {
		var _p14 = A3(_elm_community$elm_time$Time_Date$isValidDate, year, month, day) ? {ctor: '_Tuple3', _0: year, _1: month, _2: day} : (A3(_elm_community$elm_time$Time_Date$isValidDate, year, month, day - 1) ? {ctor: '_Tuple3', _0: year, _1: month, _2: day - 1} : (A3(_elm_community$elm_time$Time_Date$isValidDate, year, month, day - 2) ? {ctor: '_Tuple3', _0: year, _1: month, _2: day - 2} : {ctor: '_Tuple3', _0: year, _1: month, _2: day - 3}));
		var y = _p14._0;
		var m = _p14._1;
		var d = _p14._2;
		return _elm_community$elm_time$Time_Date$Date(
			{year: y, month: m, day: d});
	});
var _elm_community$elm_time$Time_Date$date = F3(
	function (year, month, day) {
		return A3(
			_elm_community$elm_time$Time_Date$firstValid,
			year,
			_elm_community$elm_time$Time_Date$clampMonth(month),
			_elm_community$elm_time$Time_Date$clampDay(day));
	});
var _elm_community$elm_time$Time_Date$addMonths = F2(
	function (months, _p15) {
		var _p16 = _p15;
		var ms = (((_p16._0.year * 12) + _p16._0.month) - 1) + months;
		var yo = (_elm_lang$core$Native_Utils.cmp(ms, 0) < 0) ? -1 : 0;
		return A3(
			_elm_community$elm_time$Time_Date$date,
			(((ms - yo) / 12) | 0) + yo,
			A2(_elm_lang$core$Basics_ops['%'], ms, 12) + 1,
			_p16._0.day);
	});
var _elm_community$elm_time$Time_Date$fromTuple = function (_p17) {
	var _p18 = _p17;
	return A3(_elm_community$elm_time$Time_Date$date, _p18._0, _p18._1, _p18._2);
};
var _elm_community$elm_time$Time_Date$fromISO8601 = function (input) {
	var convert = function (_p19) {
		var _p20 = _p19;
		var _p23 = _p20._0;
		var _p22 = _p20._1;
		var _p21 = _p20._2;
		return A3(_elm_community$elm_time$Time_Date$isValidDate, _p23, _p22, _p21) ? _elm_community$parser_combinators$Combine$succeed(
			A3(_elm_community$elm_time$Time_Date$date, _p23, _p22, _p21)) : _elm_community$parser_combinators$Combine$fail('invalid date');
	};
	var dateTuple = A2(
		_elm_community$parser_combinators$Combine_ops['<*>'],
		A2(
			_elm_community$parser_combinators$Combine_ops['<*>'],
			A2(
				_elm_community$parser_combinators$Combine_ops['<$>'],
				F3(
					function (v0, v1, v2) {
						return {ctor: '_Tuple3', _0: v0, _1: v1, _2: v2};
					}),
				_elm_community$parser_combinators$Combine_Num$int),
			A2(
				_elm_community$parser_combinators$Combine_ops['*>'],
				_elm_community$parser_combinators$Combine$string('-'),
				A2(_elm_community$elm_time$Time_Internal$intRange, 1, 12))),
		A2(
			_elm_community$parser_combinators$Combine_ops['*>'],
			_elm_community$parser_combinators$Combine$string('-'),
			A2(_elm_community$elm_time$Time_Internal$intRange, 1, 31)));
	var _p24 = A2(
		_elm_community$parser_combinators$Combine$parse,
		A2(_elm_community$parser_combinators$Combine_ops['>>='], dateTuple, convert),
		input);
	if (_p24.ctor === 'Ok') {
		return _elm_lang$core$Result$Ok(_p24._0._2);
	} else {
		var messages = A2(_elm_lang$core$String$join, ' or ', _p24._0._2);
		return _elm_lang$core$Result$Err(
			A2(
				_elm_lang$core$Basics_ops['++'],
				'Errors encountered at position ',
				A2(
					_elm_lang$core$Basics_ops['++'],
					_elm_lang$core$Basics$toString(_p24._0._1.position),
					A2(_elm_lang$core$Basics_ops['++'], ': ', messages))));
	}
};
var _elm_community$elm_time$Time_Date$setYear = F2(
	function (year, _p25) {
		var _p26 = _p25;
		return A3(_elm_community$elm_time$Time_Date$firstValid, year, _p26._0.month, _p26._0.day);
	});
var _elm_community$elm_time$Time_Date$setMonth = F2(
	function (month, _p27) {
		var _p28 = _p27;
		return A3(
			_elm_community$elm_time$Time_Date$firstValid,
			_p28._0.year,
			_elm_community$elm_time$Time_Date$clampMonth(month),
			_p28._0.day);
	});
var _elm_community$elm_time$Time_Date$setDay = F2(
	function (day, _p29) {
		var _p30 = _p29;
		return A3(
			_elm_community$elm_time$Time_Date$firstValid,
			_p30._0.year,
			_p30._0.month,
			_elm_community$elm_time$Time_Date$clampDay(day));
	});
var _elm_community$elm_time$Time_Date$addYears = F2(
	function (years, _p31) {
		var _p32 = _p31;
		return A3(_elm_community$elm_time$Time_Date$firstValid, _p32._0.year + years, _p32._0.month, _p32._0.day);
	});
var _elm_community$elm_time$Time_Date$dateFromDays = function (ds) {
	var d400 = _elm_community$elm_time$Time_Date$daysFromYear(400);
	var y400 = (ds / d400) | 0;
	var d = A2(_elm_lang$core$Basics$rem, ds, d400);
	var year = _elm_community$elm_time$Time_Date$yearFromDays(d + 1);
	var leap = _elm_community$elm_time$Time_Date$isLeapYear(year) ? F2(
		function (x, y) {
			return x + y;
		})(1) : _elm_lang$core$Basics$identity;
	var doy = d - _elm_community$elm_time$Time_Date$daysFromYear(year);
	var _p33 = (_elm_lang$core$Native_Utils.cmp(doy, 31) < 0) ? {ctor: '_Tuple2', _0: 1, _1: doy + 1} : ((_elm_lang$core$Native_Utils.cmp(
		doy,
		leap(59)) < 0) ? {ctor: '_Tuple2', _0: 2, _1: (doy - 31) + 1} : ((_elm_lang$core$Native_Utils.cmp(
		doy,
		leap(90)) < 0) ? {
		ctor: '_Tuple2',
		_0: 3,
		_1: (doy - leap(59)) + 1
	} : ((_elm_lang$core$Native_Utils.cmp(
		doy,
		leap(120)) < 0) ? {
		ctor: '_Tuple2',
		_0: 4,
		_1: (doy - leap(90)) + 1
	} : ((_elm_lang$core$Native_Utils.cmp(
		doy,
		leap(151)) < 0) ? {
		ctor: '_Tuple2',
		_0: 5,
		_1: (doy - leap(120)) + 1
	} : ((_elm_lang$core$Native_Utils.cmp(
		doy,
		leap(181)) < 0) ? {
		ctor: '_Tuple2',
		_0: 6,
		_1: (doy - leap(151)) + 1
	} : ((_elm_lang$core$Native_Utils.cmp(
		doy,
		leap(212)) < 0) ? {
		ctor: '_Tuple2',
		_0: 7,
		_1: (doy - leap(181)) + 1
	} : ((_elm_lang$core$Native_Utils.cmp(
		doy,
		leap(243)) < 0) ? {
		ctor: '_Tuple2',
		_0: 8,
		_1: (doy - leap(212)) + 1
	} : ((_elm_lang$core$Native_Utils.cmp(
		doy,
		leap(273)) < 0) ? {
		ctor: '_Tuple2',
		_0: 9,
		_1: (doy - leap(243)) + 1
	} : ((_elm_lang$core$Native_Utils.cmp(
		doy,
		leap(304)) < 0) ? {
		ctor: '_Tuple2',
		_0: 10,
		_1: (doy - leap(273)) + 1
	} : ((_elm_lang$core$Native_Utils.cmp(
		doy,
		leap(334)) < 0) ? {
		ctor: '_Tuple2',
		_0: 11,
		_1: (doy - leap(304)) + 1
	} : {
		ctor: '_Tuple2',
		_0: 12,
		_1: (doy - leap(334)) + 1
	}))))))))));
	var month = _p33._0;
	var day = _p33._1;
	return _elm_community$elm_time$Time_Date$Date(
		{year: year + (y400 * 400), month: month, day: day});
};
var _elm_community$elm_time$Time_Date$addDays = F2(
	function (days, _p34) {
		var _p35 = _p34;
		return _elm_community$elm_time$Time_Date$dateFromDays(
			A2(
				F2(
					function (x, y) {
						return x + y;
					}),
				days,
				A3(_elm_community$elm_time$Time_Date$daysFromYearMonthDay, _p35._0.year, _p35._0.month, _p35._0.day)));
	});
var _elm_community$elm_time$Time_Date$Sun = {ctor: 'Sun'};
var _elm_community$elm_time$Time_Date$Sat = {ctor: 'Sat'};
var _elm_community$elm_time$Time_Date$Fri = {ctor: 'Fri'};
var _elm_community$elm_time$Time_Date$Thu = {ctor: 'Thu'};
var _elm_community$elm_time$Time_Date$Wed = {ctor: 'Wed'};
var _elm_community$elm_time$Time_Date$Tue = {ctor: 'Tue'};
var _elm_community$elm_time$Time_Date$Mon = {ctor: 'Mon'};
var _elm_community$elm_time$Time_Date$weekday = function (_p36) {
	var _p37 = _p36;
	var _p39 = _p37._0.year;
	var _p38 = _p37._0.month;
	var y = (_elm_lang$core$Native_Utils.cmp(_p38, 3) < 0) ? (_p39 - 1) : _p39;
	var m = _elm_lang$core$Native_Utils.eq(_p38, 1) ? 0 : (_elm_lang$core$Native_Utils.eq(_p38, 2) ? 3 : (_elm_lang$core$Native_Utils.eq(_p38, 3) ? 2 : (_elm_lang$core$Native_Utils.eq(_p38, 4) ? 5 : (_elm_lang$core$Native_Utils.eq(_p38, 5) ? 0 : (_elm_lang$core$Native_Utils.eq(_p38, 6) ? 3 : (_elm_lang$core$Native_Utils.eq(_p38, 7) ? 5 : (_elm_lang$core$Native_Utils.eq(_p38, 8) ? 1 : (_elm_lang$core$Native_Utils.eq(_p38, 9) ? 4 : (_elm_lang$core$Native_Utils.eq(_p38, 10) ? 6 : (_elm_lang$core$Native_Utils.eq(_p38, 11) ? 2 : 4))))))))));
	var d = A2(_elm_lang$core$Basics_ops['%'], ((((y + ((y / 4) | 0)) - ((y / 100) | 0)) + ((y / 400) | 0)) + m) + _p37._0.day, 7);
	return _elm_lang$core$Native_Utils.eq(d, 0) ? _elm_community$elm_time$Time_Date$Sun : (_elm_lang$core$Native_Utils.eq(d, 1) ? _elm_community$elm_time$Time_Date$Mon : (_elm_lang$core$Native_Utils.eq(d, 2) ? _elm_community$elm_time$Time_Date$Tue : (_elm_lang$core$Native_Utils.eq(d, 3) ? _elm_community$elm_time$Time_Date$Wed : (_elm_lang$core$Native_Utils.eq(d, 4) ? _elm_community$elm_time$Time_Date$Thu : (_elm_lang$core$Native_Utils.eq(d, 5) ? _elm_community$elm_time$Time_Date$Fri : _elm_community$elm_time$Time_Date$Sat)))));
};

var _elm_lang$core$Dict$foldr = F3(
	function (f, acc, t) {
		foldr:
		while (true) {
			var _p0 = t;
			if (_p0.ctor === 'RBEmpty_elm_builtin') {
				return acc;
			} else {
				var _v1 = f,
					_v2 = A3(
					f,
					_p0._1,
					_p0._2,
					A3(_elm_lang$core$Dict$foldr, f, acc, _p0._4)),
					_v3 = _p0._3;
				f = _v1;
				acc = _v2;
				t = _v3;
				continue foldr;
			}
		}
	});
var _elm_lang$core$Dict$keys = function (dict) {
	return A3(
		_elm_lang$core$Dict$foldr,
		F3(
			function (key, value, keyList) {
				return {ctor: '::', _0: key, _1: keyList};
			}),
		{ctor: '[]'},
		dict);
};
var _elm_lang$core$Dict$values = function (dict) {
	return A3(
		_elm_lang$core$Dict$foldr,
		F3(
			function (key, value, valueList) {
				return {ctor: '::', _0: value, _1: valueList};
			}),
		{ctor: '[]'},
		dict);
};
var _elm_lang$core$Dict$toList = function (dict) {
	return A3(
		_elm_lang$core$Dict$foldr,
		F3(
			function (key, value, list) {
				return {
					ctor: '::',
					_0: {ctor: '_Tuple2', _0: key, _1: value},
					_1: list
				};
			}),
		{ctor: '[]'},
		dict);
};
var _elm_lang$core$Dict$foldl = F3(
	function (f, acc, dict) {
		foldl:
		while (true) {
			var _p1 = dict;
			if (_p1.ctor === 'RBEmpty_elm_builtin') {
				return acc;
			} else {
				var _v5 = f,
					_v6 = A3(
					f,
					_p1._1,
					_p1._2,
					A3(_elm_lang$core$Dict$foldl, f, acc, _p1._3)),
					_v7 = _p1._4;
				f = _v5;
				acc = _v6;
				dict = _v7;
				continue foldl;
			}
		}
	});
var _elm_lang$core$Dict$merge = F6(
	function (leftStep, bothStep, rightStep, leftDict, rightDict, initialResult) {
		var stepState = F3(
			function (rKey, rValue, _p2) {
				stepState:
				while (true) {
					var _p3 = _p2;
					var _p9 = _p3._1;
					var _p8 = _p3._0;
					var _p4 = _p8;
					if (_p4.ctor === '[]') {
						return {
							ctor: '_Tuple2',
							_0: _p8,
							_1: A3(rightStep, rKey, rValue, _p9)
						};
					} else {
						var _p7 = _p4._1;
						var _p6 = _p4._0._1;
						var _p5 = _p4._0._0;
						if (_elm_lang$core$Native_Utils.cmp(_p5, rKey) < 0) {
							var _v10 = rKey,
								_v11 = rValue,
								_v12 = {
								ctor: '_Tuple2',
								_0: _p7,
								_1: A3(leftStep, _p5, _p6, _p9)
							};
							rKey = _v10;
							rValue = _v11;
							_p2 = _v12;
							continue stepState;
						} else {
							if (_elm_lang$core$Native_Utils.cmp(_p5, rKey) > 0) {
								return {
									ctor: '_Tuple2',
									_0: _p8,
									_1: A3(rightStep, rKey, rValue, _p9)
								};
							} else {
								return {
									ctor: '_Tuple2',
									_0: _p7,
									_1: A4(bothStep, _p5, _p6, rValue, _p9)
								};
							}
						}
					}
				}
			});
		var _p10 = A3(
			_elm_lang$core$Dict$foldl,
			stepState,
			{
				ctor: '_Tuple2',
				_0: _elm_lang$core$Dict$toList(leftDict),
				_1: initialResult
			},
			rightDict);
		var leftovers = _p10._0;
		var intermediateResult = _p10._1;
		return A3(
			_elm_lang$core$List$foldl,
			F2(
				function (_p11, result) {
					var _p12 = _p11;
					return A3(leftStep, _p12._0, _p12._1, result);
				}),
			intermediateResult,
			leftovers);
	});
var _elm_lang$core$Dict$reportRemBug = F4(
	function (msg, c, lgot, rgot) {
		return _elm_lang$core$Native_Debug.crash(
			_elm_lang$core$String$concat(
				{
					ctor: '::',
					_0: 'Internal red-black tree invariant violated, expected ',
					_1: {
						ctor: '::',
						_0: msg,
						_1: {
							ctor: '::',
							_0: ' and got ',
							_1: {
								ctor: '::',
								_0: _elm_lang$core$Basics$toString(c),
								_1: {
									ctor: '::',
									_0: '/',
									_1: {
										ctor: '::',
										_0: lgot,
										_1: {
											ctor: '::',
											_0: '/',
											_1: {
												ctor: '::',
												_0: rgot,
												_1: {
													ctor: '::',
													_0: '\nPlease report this bug to <https://github.com/elm-lang/core/issues>',
													_1: {ctor: '[]'}
												}
											}
										}
									}
								}
							}
						}
					}
				}));
	});
var _elm_lang$core$Dict$isBBlack = function (dict) {
	var _p13 = dict;
	_v14_2:
	do {
		if (_p13.ctor === 'RBNode_elm_builtin') {
			if (_p13._0.ctor === 'BBlack') {
				return true;
			} else {
				break _v14_2;
			}
		} else {
			if (_p13._0.ctor === 'LBBlack') {
				return true;
			} else {
				break _v14_2;
			}
		}
	} while(false);
	return false;
};
var _elm_lang$core$Dict$sizeHelp = F2(
	function (n, dict) {
		sizeHelp:
		while (true) {
			var _p14 = dict;
			if (_p14.ctor === 'RBEmpty_elm_builtin') {
				return n;
			} else {
				var _v16 = A2(_elm_lang$core$Dict$sizeHelp, n + 1, _p14._4),
					_v17 = _p14._3;
				n = _v16;
				dict = _v17;
				continue sizeHelp;
			}
		}
	});
var _elm_lang$core$Dict$size = function (dict) {
	return A2(_elm_lang$core$Dict$sizeHelp, 0, dict);
};
var _elm_lang$core$Dict$get = F2(
	function (targetKey, dict) {
		get:
		while (true) {
			var _p15 = dict;
			if (_p15.ctor === 'RBEmpty_elm_builtin') {
				return _elm_lang$core$Maybe$Nothing;
			} else {
				var _p16 = A2(_elm_lang$core$Basics$compare, targetKey, _p15._1);
				switch (_p16.ctor) {
					case 'LT':
						var _v20 = targetKey,
							_v21 = _p15._3;
						targetKey = _v20;
						dict = _v21;
						continue get;
					case 'EQ':
						return _elm_lang$core$Maybe$Just(_p15._2);
					default:
						var _v22 = targetKey,
							_v23 = _p15._4;
						targetKey = _v22;
						dict = _v23;
						continue get;
				}
			}
		}
	});
var _elm_lang$core$Dict$member = F2(
	function (key, dict) {
		var _p17 = A2(_elm_lang$core$Dict$get, key, dict);
		if (_p17.ctor === 'Just') {
			return true;
		} else {
			return false;
		}
	});
var _elm_lang$core$Dict$maxWithDefault = F3(
	function (k, v, r) {
		maxWithDefault:
		while (true) {
			var _p18 = r;
			if (_p18.ctor === 'RBEmpty_elm_builtin') {
				return {ctor: '_Tuple2', _0: k, _1: v};
			} else {
				var _v26 = _p18._1,
					_v27 = _p18._2,
					_v28 = _p18._4;
				k = _v26;
				v = _v27;
				r = _v28;
				continue maxWithDefault;
			}
		}
	});
var _elm_lang$core$Dict$NBlack = {ctor: 'NBlack'};
var _elm_lang$core$Dict$BBlack = {ctor: 'BBlack'};
var _elm_lang$core$Dict$Black = {ctor: 'Black'};
var _elm_lang$core$Dict$blackish = function (t) {
	var _p19 = t;
	if (_p19.ctor === 'RBNode_elm_builtin') {
		var _p20 = _p19._0;
		return _elm_lang$core$Native_Utils.eq(_p20, _elm_lang$core$Dict$Black) || _elm_lang$core$Native_Utils.eq(_p20, _elm_lang$core$Dict$BBlack);
	} else {
		return true;
	}
};
var _elm_lang$core$Dict$Red = {ctor: 'Red'};
var _elm_lang$core$Dict$moreBlack = function (color) {
	var _p21 = color;
	switch (_p21.ctor) {
		case 'Black':
			return _elm_lang$core$Dict$BBlack;
		case 'Red':
			return _elm_lang$core$Dict$Black;
		case 'NBlack':
			return _elm_lang$core$Dict$Red;
		default:
			return _elm_lang$core$Native_Debug.crash('Can\'t make a double black node more black!');
	}
};
var _elm_lang$core$Dict$lessBlack = function (color) {
	var _p22 = color;
	switch (_p22.ctor) {
		case 'BBlack':
			return _elm_lang$core$Dict$Black;
		case 'Black':
			return _elm_lang$core$Dict$Red;
		case 'Red':
			return _elm_lang$core$Dict$NBlack;
		default:
			return _elm_lang$core$Native_Debug.crash('Can\'t make a negative black node less black!');
	}
};
var _elm_lang$core$Dict$LBBlack = {ctor: 'LBBlack'};
var _elm_lang$core$Dict$LBlack = {ctor: 'LBlack'};
var _elm_lang$core$Dict$RBEmpty_elm_builtin = function (a) {
	return {ctor: 'RBEmpty_elm_builtin', _0: a};
};
var _elm_lang$core$Dict$empty = _elm_lang$core$Dict$RBEmpty_elm_builtin(_elm_lang$core$Dict$LBlack);
var _elm_lang$core$Dict$isEmpty = function (dict) {
	return _elm_lang$core$Native_Utils.eq(dict, _elm_lang$core$Dict$empty);
};
var _elm_lang$core$Dict$RBNode_elm_builtin = F5(
	function (a, b, c, d, e) {
		return {ctor: 'RBNode_elm_builtin', _0: a, _1: b, _2: c, _3: d, _4: e};
	});
var _elm_lang$core$Dict$ensureBlackRoot = function (dict) {
	var _p23 = dict;
	if ((_p23.ctor === 'RBNode_elm_builtin') && (_p23._0.ctor === 'Red')) {
		return A5(_elm_lang$core$Dict$RBNode_elm_builtin, _elm_lang$core$Dict$Black, _p23._1, _p23._2, _p23._3, _p23._4);
	} else {
		return dict;
	}
};
var _elm_lang$core$Dict$lessBlackTree = function (dict) {
	var _p24 = dict;
	if (_p24.ctor === 'RBNode_elm_builtin') {
		return A5(
			_elm_lang$core$Dict$RBNode_elm_builtin,
			_elm_lang$core$Dict$lessBlack(_p24._0),
			_p24._1,
			_p24._2,
			_p24._3,
			_p24._4);
	} else {
		return _elm_lang$core$Dict$RBEmpty_elm_builtin(_elm_lang$core$Dict$LBlack);
	}
};
var _elm_lang$core$Dict$balancedTree = function (col) {
	return function (xk) {
		return function (xv) {
			return function (yk) {
				return function (yv) {
					return function (zk) {
						return function (zv) {
							return function (a) {
								return function (b) {
									return function (c) {
										return function (d) {
											return A5(
												_elm_lang$core$Dict$RBNode_elm_builtin,
												_elm_lang$core$Dict$lessBlack(col),
												yk,
												yv,
												A5(_elm_lang$core$Dict$RBNode_elm_builtin, _elm_lang$core$Dict$Black, xk, xv, a, b),
												A5(_elm_lang$core$Dict$RBNode_elm_builtin, _elm_lang$core$Dict$Black, zk, zv, c, d));
										};
									};
								};
							};
						};
					};
				};
			};
		};
	};
};
var _elm_lang$core$Dict$blacken = function (t) {
	var _p25 = t;
	if (_p25.ctor === 'RBEmpty_elm_builtin') {
		return _elm_lang$core$Dict$RBEmpty_elm_builtin(_elm_lang$core$Dict$LBlack);
	} else {
		return A5(_elm_lang$core$Dict$RBNode_elm_builtin, _elm_lang$core$Dict$Black, _p25._1, _p25._2, _p25._3, _p25._4);
	}
};
var _elm_lang$core$Dict$redden = function (t) {
	var _p26 = t;
	if (_p26.ctor === 'RBEmpty_elm_builtin') {
		return _elm_lang$core$Native_Debug.crash('can\'t make a Leaf red');
	} else {
		return A5(_elm_lang$core$Dict$RBNode_elm_builtin, _elm_lang$core$Dict$Red, _p26._1, _p26._2, _p26._3, _p26._4);
	}
};
var _elm_lang$core$Dict$balanceHelp = function (tree) {
	var _p27 = tree;
	_v36_6:
	do {
		_v36_5:
		do {
			_v36_4:
			do {
				_v36_3:
				do {
					_v36_2:
					do {
						_v36_1:
						do {
							_v36_0:
							do {
								if (_p27.ctor === 'RBNode_elm_builtin') {
									if (_p27._3.ctor === 'RBNode_elm_builtin') {
										if (_p27._4.ctor === 'RBNode_elm_builtin') {
											switch (_p27._3._0.ctor) {
												case 'Red':
													switch (_p27._4._0.ctor) {
														case 'Red':
															if ((_p27._3._3.ctor === 'RBNode_elm_builtin') && (_p27._3._3._0.ctor === 'Red')) {
																break _v36_0;
															} else {
																if ((_p27._3._4.ctor === 'RBNode_elm_builtin') && (_p27._3._4._0.ctor === 'Red')) {
																	break _v36_1;
																} else {
																	if ((_p27._4._3.ctor === 'RBNode_elm_builtin') && (_p27._4._3._0.ctor === 'Red')) {
																		break _v36_2;
																	} else {
																		if ((_p27._4._4.ctor === 'RBNode_elm_builtin') && (_p27._4._4._0.ctor === 'Red')) {
																			break _v36_3;
																		} else {
																			break _v36_6;
																		}
																	}
																}
															}
														case 'NBlack':
															if ((_p27._3._3.ctor === 'RBNode_elm_builtin') && (_p27._3._3._0.ctor === 'Red')) {
																break _v36_0;
															} else {
																if ((_p27._3._4.ctor === 'RBNode_elm_builtin') && (_p27._3._4._0.ctor === 'Red')) {
																	break _v36_1;
																} else {
																	if (((((_p27._0.ctor === 'BBlack') && (_p27._4._3.ctor === 'RBNode_elm_builtin')) && (_p27._4._3._0.ctor === 'Black')) && (_p27._4._4.ctor === 'RBNode_elm_builtin')) && (_p27._4._4._0.ctor === 'Black')) {
																		break _v36_4;
																	} else {
																		break _v36_6;
																	}
																}
															}
														default:
															if ((_p27._3._3.ctor === 'RBNode_elm_builtin') && (_p27._3._3._0.ctor === 'Red')) {
																break _v36_0;
															} else {
																if ((_p27._3._4.ctor === 'RBNode_elm_builtin') && (_p27._3._4._0.ctor === 'Red')) {
																	break _v36_1;
																} else {
																	break _v36_6;
																}
															}
													}
												case 'NBlack':
													switch (_p27._4._0.ctor) {
														case 'Red':
															if ((_p27._4._3.ctor === 'RBNode_elm_builtin') && (_p27._4._3._0.ctor === 'Red')) {
																break _v36_2;
															} else {
																if ((_p27._4._4.ctor === 'RBNode_elm_builtin') && (_p27._4._4._0.ctor === 'Red')) {
																	break _v36_3;
																} else {
																	if (((((_p27._0.ctor === 'BBlack') && (_p27._3._3.ctor === 'RBNode_elm_builtin')) && (_p27._3._3._0.ctor === 'Black')) && (_p27._3._4.ctor === 'RBNode_elm_builtin')) && (_p27._3._4._0.ctor === 'Black')) {
																		break _v36_5;
																	} else {
																		break _v36_6;
																	}
																}
															}
														case 'NBlack':
															if (_p27._0.ctor === 'BBlack') {
																if ((((_p27._4._3.ctor === 'RBNode_elm_builtin') && (_p27._4._3._0.ctor === 'Black')) && (_p27._4._4.ctor === 'RBNode_elm_builtin')) && (_p27._4._4._0.ctor === 'Black')) {
																	break _v36_4;
																} else {
																	if ((((_p27._3._3.ctor === 'RBNode_elm_builtin') && (_p27._3._3._0.ctor === 'Black')) && (_p27._3._4.ctor === 'RBNode_elm_builtin')) && (_p27._3._4._0.ctor === 'Black')) {
																		break _v36_5;
																	} else {
																		break _v36_6;
																	}
																}
															} else {
																break _v36_6;
															}
														default:
															if (((((_p27._0.ctor === 'BBlack') && (_p27._3._3.ctor === 'RBNode_elm_builtin')) && (_p27._3._3._0.ctor === 'Black')) && (_p27._3._4.ctor === 'RBNode_elm_builtin')) && (_p27._3._4._0.ctor === 'Black')) {
																break _v36_5;
															} else {
																break _v36_6;
															}
													}
												default:
													switch (_p27._4._0.ctor) {
														case 'Red':
															if ((_p27._4._3.ctor === 'RBNode_elm_builtin') && (_p27._4._3._0.ctor === 'Red')) {
																break _v36_2;
															} else {
																if ((_p27._4._4.ctor === 'RBNode_elm_builtin') && (_p27._4._4._0.ctor === 'Red')) {
																	break _v36_3;
																} else {
																	break _v36_6;
																}
															}
														case 'NBlack':
															if (((((_p27._0.ctor === 'BBlack') && (_p27._4._3.ctor === 'RBNode_elm_builtin')) && (_p27._4._3._0.ctor === 'Black')) && (_p27._4._4.ctor === 'RBNode_elm_builtin')) && (_p27._4._4._0.ctor === 'Black')) {
																break _v36_4;
															} else {
																break _v36_6;
															}
														default:
															break _v36_6;
													}
											}
										} else {
											switch (_p27._3._0.ctor) {
												case 'Red':
													if ((_p27._3._3.ctor === 'RBNode_elm_builtin') && (_p27._3._3._0.ctor === 'Red')) {
														break _v36_0;
													} else {
														if ((_p27._3._4.ctor === 'RBNode_elm_builtin') && (_p27._3._4._0.ctor === 'Red')) {
															break _v36_1;
														} else {
															break _v36_6;
														}
													}
												case 'NBlack':
													if (((((_p27._0.ctor === 'BBlack') && (_p27._3._3.ctor === 'RBNode_elm_builtin')) && (_p27._3._3._0.ctor === 'Black')) && (_p27._3._4.ctor === 'RBNode_elm_builtin')) && (_p27._3._4._0.ctor === 'Black')) {
														break _v36_5;
													} else {
														break _v36_6;
													}
												default:
													break _v36_6;
											}
										}
									} else {
										if (_p27._4.ctor === 'RBNode_elm_builtin') {
											switch (_p27._4._0.ctor) {
												case 'Red':
													if ((_p27._4._3.ctor === 'RBNode_elm_builtin') && (_p27._4._3._0.ctor === 'Red')) {
														break _v36_2;
													} else {
														if ((_p27._4._4.ctor === 'RBNode_elm_builtin') && (_p27._4._4._0.ctor === 'Red')) {
															break _v36_3;
														} else {
															break _v36_6;
														}
													}
												case 'NBlack':
													if (((((_p27._0.ctor === 'BBlack') && (_p27._4._3.ctor === 'RBNode_elm_builtin')) && (_p27._4._3._0.ctor === 'Black')) && (_p27._4._4.ctor === 'RBNode_elm_builtin')) && (_p27._4._4._0.ctor === 'Black')) {
														break _v36_4;
													} else {
														break _v36_6;
													}
												default:
													break _v36_6;
											}
										} else {
											break _v36_6;
										}
									}
								} else {
									break _v36_6;
								}
							} while(false);
							return _elm_lang$core$Dict$balancedTree(_p27._0)(_p27._3._3._1)(_p27._3._3._2)(_p27._3._1)(_p27._3._2)(_p27._1)(_p27._2)(_p27._3._3._3)(_p27._3._3._4)(_p27._3._4)(_p27._4);
						} while(false);
						return _elm_lang$core$Dict$balancedTree(_p27._0)(_p27._3._1)(_p27._3._2)(_p27._3._4._1)(_p27._3._4._2)(_p27._1)(_p27._2)(_p27._3._3)(_p27._3._4._3)(_p27._3._4._4)(_p27._4);
					} while(false);
					return _elm_lang$core$Dict$balancedTree(_p27._0)(_p27._1)(_p27._2)(_p27._4._3._1)(_p27._4._3._2)(_p27._4._1)(_p27._4._2)(_p27._3)(_p27._4._3._3)(_p27._4._3._4)(_p27._4._4);
				} while(false);
				return _elm_lang$core$Dict$balancedTree(_p27._0)(_p27._1)(_p27._2)(_p27._4._1)(_p27._4._2)(_p27._4._4._1)(_p27._4._4._2)(_p27._3)(_p27._4._3)(_p27._4._4._3)(_p27._4._4._4);
			} while(false);
			return A5(
				_elm_lang$core$Dict$RBNode_elm_builtin,
				_elm_lang$core$Dict$Black,
				_p27._4._3._1,
				_p27._4._3._2,
				A5(_elm_lang$core$Dict$RBNode_elm_builtin, _elm_lang$core$Dict$Black, _p27._1, _p27._2, _p27._3, _p27._4._3._3),
				A5(
					_elm_lang$core$Dict$balance,
					_elm_lang$core$Dict$Black,
					_p27._4._1,
					_p27._4._2,
					_p27._4._3._4,
					_elm_lang$core$Dict$redden(_p27._4._4)));
		} while(false);
		return A5(
			_elm_lang$core$Dict$RBNode_elm_builtin,
			_elm_lang$core$Dict$Black,
			_p27._3._4._1,
			_p27._3._4._2,
			A5(
				_elm_lang$core$Dict$balance,
				_elm_lang$core$Dict$Black,
				_p27._3._1,
				_p27._3._2,
				_elm_lang$core$Dict$redden(_p27._3._3),
				_p27._3._4._3),
			A5(_elm_lang$core$Dict$RBNode_elm_builtin, _elm_lang$core$Dict$Black, _p27._1, _p27._2, _p27._3._4._4, _p27._4));
	} while(false);
	return tree;
};
var _elm_lang$core$Dict$balance = F5(
	function (c, k, v, l, r) {
		var tree = A5(_elm_lang$core$Dict$RBNode_elm_builtin, c, k, v, l, r);
		return _elm_lang$core$Dict$blackish(tree) ? _elm_lang$core$Dict$balanceHelp(tree) : tree;
	});
var _elm_lang$core$Dict$bubble = F5(
	function (c, k, v, l, r) {
		return (_elm_lang$core$Dict$isBBlack(l) || _elm_lang$core$Dict$isBBlack(r)) ? A5(
			_elm_lang$core$Dict$balance,
			_elm_lang$core$Dict$moreBlack(c),
			k,
			v,
			_elm_lang$core$Dict$lessBlackTree(l),
			_elm_lang$core$Dict$lessBlackTree(r)) : A5(_elm_lang$core$Dict$RBNode_elm_builtin, c, k, v, l, r);
	});
var _elm_lang$core$Dict$removeMax = F5(
	function (c, k, v, l, r) {
		var _p28 = r;
		if (_p28.ctor === 'RBEmpty_elm_builtin') {
			return A3(_elm_lang$core$Dict$rem, c, l, r);
		} else {
			return A5(
				_elm_lang$core$Dict$bubble,
				c,
				k,
				v,
				l,
				A5(_elm_lang$core$Dict$removeMax, _p28._0, _p28._1, _p28._2, _p28._3, _p28._4));
		}
	});
var _elm_lang$core$Dict$rem = F3(
	function (color, left, right) {
		var _p29 = {ctor: '_Tuple2', _0: left, _1: right};
		if (_p29._0.ctor === 'RBEmpty_elm_builtin') {
			if (_p29._1.ctor === 'RBEmpty_elm_builtin') {
				var _p30 = color;
				switch (_p30.ctor) {
					case 'Red':
						return _elm_lang$core$Dict$RBEmpty_elm_builtin(_elm_lang$core$Dict$LBlack);
					case 'Black':
						return _elm_lang$core$Dict$RBEmpty_elm_builtin(_elm_lang$core$Dict$LBBlack);
					default:
						return _elm_lang$core$Native_Debug.crash('cannot have bblack or nblack nodes at this point');
				}
			} else {
				var _p33 = _p29._1._0;
				var _p32 = _p29._0._0;
				var _p31 = {ctor: '_Tuple3', _0: color, _1: _p32, _2: _p33};
				if ((((_p31.ctor === '_Tuple3') && (_p31._0.ctor === 'Black')) && (_p31._1.ctor === 'LBlack')) && (_p31._2.ctor === 'Red')) {
					return A5(_elm_lang$core$Dict$RBNode_elm_builtin, _elm_lang$core$Dict$Black, _p29._1._1, _p29._1._2, _p29._1._3, _p29._1._4);
				} else {
					return A4(
						_elm_lang$core$Dict$reportRemBug,
						'Black/LBlack/Red',
						color,
						_elm_lang$core$Basics$toString(_p32),
						_elm_lang$core$Basics$toString(_p33));
				}
			}
		} else {
			if (_p29._1.ctor === 'RBEmpty_elm_builtin') {
				var _p36 = _p29._1._0;
				var _p35 = _p29._0._0;
				var _p34 = {ctor: '_Tuple3', _0: color, _1: _p35, _2: _p36};
				if ((((_p34.ctor === '_Tuple3') && (_p34._0.ctor === 'Black')) && (_p34._1.ctor === 'Red')) && (_p34._2.ctor === 'LBlack')) {
					return A5(_elm_lang$core$Dict$RBNode_elm_builtin, _elm_lang$core$Dict$Black, _p29._0._1, _p29._0._2, _p29._0._3, _p29._0._4);
				} else {
					return A4(
						_elm_lang$core$Dict$reportRemBug,
						'Black/Red/LBlack',
						color,
						_elm_lang$core$Basics$toString(_p35),
						_elm_lang$core$Basics$toString(_p36));
				}
			} else {
				var _p40 = _p29._0._2;
				var _p39 = _p29._0._4;
				var _p38 = _p29._0._1;
				var newLeft = A5(_elm_lang$core$Dict$removeMax, _p29._0._0, _p38, _p40, _p29._0._3, _p39);
				var _p37 = A3(_elm_lang$core$Dict$maxWithDefault, _p38, _p40, _p39);
				var k = _p37._0;
				var v = _p37._1;
				return A5(_elm_lang$core$Dict$bubble, color, k, v, newLeft, right);
			}
		}
	});
var _elm_lang$core$Dict$map = F2(
	function (f, dict) {
		var _p41 = dict;
		if (_p41.ctor === 'RBEmpty_elm_builtin') {
			return _elm_lang$core$Dict$RBEmpty_elm_builtin(_elm_lang$core$Dict$LBlack);
		} else {
			var _p42 = _p41._1;
			return A5(
				_elm_lang$core$Dict$RBNode_elm_builtin,
				_p41._0,
				_p42,
				A2(f, _p42, _p41._2),
				A2(_elm_lang$core$Dict$map, f, _p41._3),
				A2(_elm_lang$core$Dict$map, f, _p41._4));
		}
	});
var _elm_lang$core$Dict$Same = {ctor: 'Same'};
var _elm_lang$core$Dict$Remove = {ctor: 'Remove'};
var _elm_lang$core$Dict$Insert = {ctor: 'Insert'};
var _elm_lang$core$Dict$update = F3(
	function (k, alter, dict) {
		var up = function (dict) {
			var _p43 = dict;
			if (_p43.ctor === 'RBEmpty_elm_builtin') {
				var _p44 = alter(_elm_lang$core$Maybe$Nothing);
				if (_p44.ctor === 'Nothing') {
					return {ctor: '_Tuple2', _0: _elm_lang$core$Dict$Same, _1: _elm_lang$core$Dict$empty};
				} else {
					return {
						ctor: '_Tuple2',
						_0: _elm_lang$core$Dict$Insert,
						_1: A5(_elm_lang$core$Dict$RBNode_elm_builtin, _elm_lang$core$Dict$Red, k, _p44._0, _elm_lang$core$Dict$empty, _elm_lang$core$Dict$empty)
					};
				}
			} else {
				var _p55 = _p43._2;
				var _p54 = _p43._4;
				var _p53 = _p43._3;
				var _p52 = _p43._1;
				var _p51 = _p43._0;
				var _p45 = A2(_elm_lang$core$Basics$compare, k, _p52);
				switch (_p45.ctor) {
					case 'EQ':
						var _p46 = alter(
							_elm_lang$core$Maybe$Just(_p55));
						if (_p46.ctor === 'Nothing') {
							return {
								ctor: '_Tuple2',
								_0: _elm_lang$core$Dict$Remove,
								_1: A3(_elm_lang$core$Dict$rem, _p51, _p53, _p54)
							};
						} else {
							return {
								ctor: '_Tuple2',
								_0: _elm_lang$core$Dict$Same,
								_1: A5(_elm_lang$core$Dict$RBNode_elm_builtin, _p51, _p52, _p46._0, _p53, _p54)
							};
						}
					case 'LT':
						var _p47 = up(_p53);
						var flag = _p47._0;
						var newLeft = _p47._1;
						var _p48 = flag;
						switch (_p48.ctor) {
							case 'Same':
								return {
									ctor: '_Tuple2',
									_0: _elm_lang$core$Dict$Same,
									_1: A5(_elm_lang$core$Dict$RBNode_elm_builtin, _p51, _p52, _p55, newLeft, _p54)
								};
							case 'Insert':
								return {
									ctor: '_Tuple2',
									_0: _elm_lang$core$Dict$Insert,
									_1: A5(_elm_lang$core$Dict$balance, _p51, _p52, _p55, newLeft, _p54)
								};
							default:
								return {
									ctor: '_Tuple2',
									_0: _elm_lang$core$Dict$Remove,
									_1: A5(_elm_lang$core$Dict$bubble, _p51, _p52, _p55, newLeft, _p54)
								};
						}
					default:
						var _p49 = up(_p54);
						var flag = _p49._0;
						var newRight = _p49._1;
						var _p50 = flag;
						switch (_p50.ctor) {
							case 'Same':
								return {
									ctor: '_Tuple2',
									_0: _elm_lang$core$Dict$Same,
									_1: A5(_elm_lang$core$Dict$RBNode_elm_builtin, _p51, _p52, _p55, _p53, newRight)
								};
							case 'Insert':
								return {
									ctor: '_Tuple2',
									_0: _elm_lang$core$Dict$Insert,
									_1: A5(_elm_lang$core$Dict$balance, _p51, _p52, _p55, _p53, newRight)
								};
							default:
								return {
									ctor: '_Tuple2',
									_0: _elm_lang$core$Dict$Remove,
									_1: A5(_elm_lang$core$Dict$bubble, _p51, _p52, _p55, _p53, newRight)
								};
						}
				}
			}
		};
		var _p56 = up(dict);
		var flag = _p56._0;
		var updatedDict = _p56._1;
		var _p57 = flag;
		switch (_p57.ctor) {
			case 'Same':
				return updatedDict;
			case 'Insert':
				return _elm_lang$core$Dict$ensureBlackRoot(updatedDict);
			default:
				return _elm_lang$core$Dict$blacken(updatedDict);
		}
	});
var _elm_lang$core$Dict$insert = F3(
	function (key, value, dict) {
		return A3(
			_elm_lang$core$Dict$update,
			key,
			_elm_lang$core$Basics$always(
				_elm_lang$core$Maybe$Just(value)),
			dict);
	});
var _elm_lang$core$Dict$singleton = F2(
	function (key, value) {
		return A3(_elm_lang$core$Dict$insert, key, value, _elm_lang$core$Dict$empty);
	});
var _elm_lang$core$Dict$union = F2(
	function (t1, t2) {
		return A3(_elm_lang$core$Dict$foldl, _elm_lang$core$Dict$insert, t2, t1);
	});
var _elm_lang$core$Dict$filter = F2(
	function (predicate, dictionary) {
		var add = F3(
			function (key, value, dict) {
				return A2(predicate, key, value) ? A3(_elm_lang$core$Dict$insert, key, value, dict) : dict;
			});
		return A3(_elm_lang$core$Dict$foldl, add, _elm_lang$core$Dict$empty, dictionary);
	});
var _elm_lang$core$Dict$intersect = F2(
	function (t1, t2) {
		return A2(
			_elm_lang$core$Dict$filter,
			F2(
				function (k, _p58) {
					return A2(_elm_lang$core$Dict$member, k, t2);
				}),
			t1);
	});
var _elm_lang$core$Dict$partition = F2(
	function (predicate, dict) {
		var add = F3(
			function (key, value, _p59) {
				var _p60 = _p59;
				var _p62 = _p60._1;
				var _p61 = _p60._0;
				return A2(predicate, key, value) ? {
					ctor: '_Tuple2',
					_0: A3(_elm_lang$core$Dict$insert, key, value, _p61),
					_1: _p62
				} : {
					ctor: '_Tuple2',
					_0: _p61,
					_1: A3(_elm_lang$core$Dict$insert, key, value, _p62)
				};
			});
		return A3(
			_elm_lang$core$Dict$foldl,
			add,
			{ctor: '_Tuple2', _0: _elm_lang$core$Dict$empty, _1: _elm_lang$core$Dict$empty},
			dict);
	});
var _elm_lang$core$Dict$fromList = function (assocs) {
	return A3(
		_elm_lang$core$List$foldl,
		F2(
			function (_p63, dict) {
				var _p64 = _p63;
				return A3(_elm_lang$core$Dict$insert, _p64._0, _p64._1, dict);
			}),
		_elm_lang$core$Dict$empty,
		assocs);
};
var _elm_lang$core$Dict$remove = F2(
	function (key, dict) {
		return A3(
			_elm_lang$core$Dict$update,
			key,
			_elm_lang$core$Basics$always(_elm_lang$core$Maybe$Nothing),
			dict);
	});
var _elm_lang$core$Dict$diff = F2(
	function (t1, t2) {
		return A3(
			_elm_lang$core$Dict$foldl,
			F3(
				function (k, v, t) {
					return A2(_elm_lang$core$Dict$remove, k, t);
				}),
			t1,
			t2);
	});

//import Native.Scheduler //

var _elm_lang$core$Native_Time = function() {

var now = _elm_lang$core$Native_Scheduler.nativeBinding(function(callback)
{
	callback(_elm_lang$core$Native_Scheduler.succeed(Date.now()));
});

function setInterval_(interval, task)
{
	return _elm_lang$core$Native_Scheduler.nativeBinding(function(callback)
	{
		var id = setInterval(function() {
			_elm_lang$core$Native_Scheduler.rawSpawn(task);
		}, interval);

		return function() { clearInterval(id); };
	});
}

return {
	now: now,
	setInterval_: F2(setInterval_)
};

}();
var _elm_lang$core$Task$onError = _elm_lang$core$Native_Scheduler.onError;
var _elm_lang$core$Task$andThen = _elm_lang$core$Native_Scheduler.andThen;
var _elm_lang$core$Task$spawnCmd = F2(
	function (router, _p0) {
		var _p1 = _p0;
		return _elm_lang$core$Native_Scheduler.spawn(
			A2(
				_elm_lang$core$Task$andThen,
				_elm_lang$core$Platform$sendToApp(router),
				_p1._0));
	});
var _elm_lang$core$Task$fail = _elm_lang$core$Native_Scheduler.fail;
var _elm_lang$core$Task$mapError = F2(
	function (convert, task) {
		return A2(
			_elm_lang$core$Task$onError,
			function (_p2) {
				return _elm_lang$core$Task$fail(
					convert(_p2));
			},
			task);
	});
var _elm_lang$core$Task$succeed = _elm_lang$core$Native_Scheduler.succeed;
var _elm_lang$core$Task$map = F2(
	function (func, taskA) {
		return A2(
			_elm_lang$core$Task$andThen,
			function (a) {
				return _elm_lang$core$Task$succeed(
					func(a));
			},
			taskA);
	});
var _elm_lang$core$Task$map2 = F3(
	function (func, taskA, taskB) {
		return A2(
			_elm_lang$core$Task$andThen,
			function (a) {
				return A2(
					_elm_lang$core$Task$andThen,
					function (b) {
						return _elm_lang$core$Task$succeed(
							A2(func, a, b));
					},
					taskB);
			},
			taskA);
	});
var _elm_lang$core$Task$map3 = F4(
	function (func, taskA, taskB, taskC) {
		return A2(
			_elm_lang$core$Task$andThen,
			function (a) {
				return A2(
					_elm_lang$core$Task$andThen,
					function (b) {
						return A2(
							_elm_lang$core$Task$andThen,
							function (c) {
								return _elm_lang$core$Task$succeed(
									A3(func, a, b, c));
							},
							taskC);
					},
					taskB);
			},
			taskA);
	});
var _elm_lang$core$Task$map4 = F5(
	function (func, taskA, taskB, taskC, taskD) {
		return A2(
			_elm_lang$core$Task$andThen,
			function (a) {
				return A2(
					_elm_lang$core$Task$andThen,
					function (b) {
						return A2(
							_elm_lang$core$Task$andThen,
							function (c) {
								return A2(
									_elm_lang$core$Task$andThen,
									function (d) {
										return _elm_lang$core$Task$succeed(
											A4(func, a, b, c, d));
									},
									taskD);
							},
							taskC);
					},
					taskB);
			},
			taskA);
	});
var _elm_lang$core$Task$map5 = F6(
	function (func, taskA, taskB, taskC, taskD, taskE) {
		return A2(
			_elm_lang$core$Task$andThen,
			function (a) {
				return A2(
					_elm_lang$core$Task$andThen,
					function (b) {
						return A2(
							_elm_lang$core$Task$andThen,
							function (c) {
								return A2(
									_elm_lang$core$Task$andThen,
									function (d) {
										return A2(
											_elm_lang$core$Task$andThen,
											function (e) {
												return _elm_lang$core$Task$succeed(
													A5(func, a, b, c, d, e));
											},
											taskE);
									},
									taskD);
							},
							taskC);
					},
					taskB);
			},
			taskA);
	});
var _elm_lang$core$Task$sequence = function (tasks) {
	var _p3 = tasks;
	if (_p3.ctor === '[]') {
		return _elm_lang$core$Task$succeed(
			{ctor: '[]'});
	} else {
		return A3(
			_elm_lang$core$Task$map2,
			F2(
				function (x, y) {
					return {ctor: '::', _0: x, _1: y};
				}),
			_p3._0,
			_elm_lang$core$Task$sequence(_p3._1));
	}
};
var _elm_lang$core$Task$onEffects = F3(
	function (router, commands, state) {
		return A2(
			_elm_lang$core$Task$map,
			function (_p4) {
				return {ctor: '_Tuple0'};
			},
			_elm_lang$core$Task$sequence(
				A2(
					_elm_lang$core$List$map,
					_elm_lang$core$Task$spawnCmd(router),
					commands)));
	});
var _elm_lang$core$Task$init = _elm_lang$core$Task$succeed(
	{ctor: '_Tuple0'});
var _elm_lang$core$Task$onSelfMsg = F3(
	function (_p7, _p6, _p5) {
		return _elm_lang$core$Task$succeed(
			{ctor: '_Tuple0'});
	});
var _elm_lang$core$Task$command = _elm_lang$core$Native_Platform.leaf('Task');
var _elm_lang$core$Task$Perform = function (a) {
	return {ctor: 'Perform', _0: a};
};
var _elm_lang$core$Task$perform = F2(
	function (toMessage, task) {
		return _elm_lang$core$Task$command(
			_elm_lang$core$Task$Perform(
				A2(_elm_lang$core$Task$map, toMessage, task)));
	});
var _elm_lang$core$Task$attempt = F2(
	function (resultToMessage, task) {
		return _elm_lang$core$Task$command(
			_elm_lang$core$Task$Perform(
				A2(
					_elm_lang$core$Task$onError,
					function (_p8) {
						return _elm_lang$core$Task$succeed(
							resultToMessage(
								_elm_lang$core$Result$Err(_p8)));
					},
					A2(
						_elm_lang$core$Task$andThen,
						function (_p9) {
							return _elm_lang$core$Task$succeed(
								resultToMessage(
									_elm_lang$core$Result$Ok(_p9)));
						},
						task))));
	});
var _elm_lang$core$Task$cmdMap = F2(
	function (tagger, _p10) {
		var _p11 = _p10;
		return _elm_lang$core$Task$Perform(
			A2(_elm_lang$core$Task$map, tagger, _p11._0));
	});
_elm_lang$core$Native_Platform.effectManagers['Task'] = {pkg: 'elm-lang/core', init: _elm_lang$core$Task$init, onEffects: _elm_lang$core$Task$onEffects, onSelfMsg: _elm_lang$core$Task$onSelfMsg, tag: 'cmd', cmdMap: _elm_lang$core$Task$cmdMap};

var _elm_lang$core$Time$setInterval = _elm_lang$core$Native_Time.setInterval_;
var _elm_lang$core$Time$spawnHelp = F3(
	function (router, intervals, processes) {
		var _p0 = intervals;
		if (_p0.ctor === '[]') {
			return _elm_lang$core$Task$succeed(processes);
		} else {
			var _p1 = _p0._0;
			var spawnRest = function (id) {
				return A3(
					_elm_lang$core$Time$spawnHelp,
					router,
					_p0._1,
					A3(_elm_lang$core$Dict$insert, _p1, id, processes));
			};
			var spawnTimer = _elm_lang$core$Native_Scheduler.spawn(
				A2(
					_elm_lang$core$Time$setInterval,
					_p1,
					A2(_elm_lang$core$Platform$sendToSelf, router, _p1)));
			return A2(_elm_lang$core$Task$andThen, spawnRest, spawnTimer);
		}
	});
var _elm_lang$core$Time$addMySub = F2(
	function (_p2, state) {
		var _p3 = _p2;
		var _p6 = _p3._1;
		var _p5 = _p3._0;
		var _p4 = A2(_elm_lang$core$Dict$get, _p5, state);
		if (_p4.ctor === 'Nothing') {
			return A3(
				_elm_lang$core$Dict$insert,
				_p5,
				{
					ctor: '::',
					_0: _p6,
					_1: {ctor: '[]'}
				},
				state);
		} else {
			return A3(
				_elm_lang$core$Dict$insert,
				_p5,
				{ctor: '::', _0: _p6, _1: _p4._0},
				state);
		}
	});
var _elm_lang$core$Time$inMilliseconds = function (t) {
	return t;
};
var _elm_lang$core$Time$millisecond = 1;
var _elm_lang$core$Time$second = 1000 * _elm_lang$core$Time$millisecond;
var _elm_lang$core$Time$minute = 60 * _elm_lang$core$Time$second;
var _elm_lang$core$Time$hour = 60 * _elm_lang$core$Time$minute;
var _elm_lang$core$Time$inHours = function (t) {
	return t / _elm_lang$core$Time$hour;
};
var _elm_lang$core$Time$inMinutes = function (t) {
	return t / _elm_lang$core$Time$minute;
};
var _elm_lang$core$Time$inSeconds = function (t) {
	return t / _elm_lang$core$Time$second;
};
var _elm_lang$core$Time$now = _elm_lang$core$Native_Time.now;
var _elm_lang$core$Time$onSelfMsg = F3(
	function (router, interval, state) {
		var _p7 = A2(_elm_lang$core$Dict$get, interval, state.taggers);
		if (_p7.ctor === 'Nothing') {
			return _elm_lang$core$Task$succeed(state);
		} else {
			var tellTaggers = function (time) {
				return _elm_lang$core$Task$sequence(
					A2(
						_elm_lang$core$List$map,
						function (tagger) {
							return A2(
								_elm_lang$core$Platform$sendToApp,
								router,
								tagger(time));
						},
						_p7._0));
			};
			return A2(
				_elm_lang$core$Task$andThen,
				function (_p8) {
					return _elm_lang$core$Task$succeed(state);
				},
				A2(_elm_lang$core$Task$andThen, tellTaggers, _elm_lang$core$Time$now));
		}
	});
var _elm_lang$core$Time$subscription = _elm_lang$core$Native_Platform.leaf('Time');
var _elm_lang$core$Time$State = F2(
	function (a, b) {
		return {taggers: a, processes: b};
	});
var _elm_lang$core$Time$init = _elm_lang$core$Task$succeed(
	A2(_elm_lang$core$Time$State, _elm_lang$core$Dict$empty, _elm_lang$core$Dict$empty));
var _elm_lang$core$Time$onEffects = F3(
	function (router, subs, _p9) {
		var _p10 = _p9;
		var rightStep = F3(
			function (_p12, id, _p11) {
				var _p13 = _p11;
				return {
					ctor: '_Tuple3',
					_0: _p13._0,
					_1: _p13._1,
					_2: A2(
						_elm_lang$core$Task$andThen,
						function (_p14) {
							return _p13._2;
						},
						_elm_lang$core$Native_Scheduler.kill(id))
				};
			});
		var bothStep = F4(
			function (interval, taggers, id, _p15) {
				var _p16 = _p15;
				return {
					ctor: '_Tuple3',
					_0: _p16._0,
					_1: A3(_elm_lang$core$Dict$insert, interval, id, _p16._1),
					_2: _p16._2
				};
			});
		var leftStep = F3(
			function (interval, taggers, _p17) {
				var _p18 = _p17;
				return {
					ctor: '_Tuple3',
					_0: {ctor: '::', _0: interval, _1: _p18._0},
					_1: _p18._1,
					_2: _p18._2
				};
			});
		var newTaggers = A3(_elm_lang$core$List$foldl, _elm_lang$core$Time$addMySub, _elm_lang$core$Dict$empty, subs);
		var _p19 = A6(
			_elm_lang$core$Dict$merge,
			leftStep,
			bothStep,
			rightStep,
			newTaggers,
			_p10.processes,
			{
				ctor: '_Tuple3',
				_0: {ctor: '[]'},
				_1: _elm_lang$core$Dict$empty,
				_2: _elm_lang$core$Task$succeed(
					{ctor: '_Tuple0'})
			});
		var spawnList = _p19._0;
		var existingDict = _p19._1;
		var killTask = _p19._2;
		return A2(
			_elm_lang$core$Task$andThen,
			function (newProcesses) {
				return _elm_lang$core$Task$succeed(
					A2(_elm_lang$core$Time$State, newTaggers, newProcesses));
			},
			A2(
				_elm_lang$core$Task$andThen,
				function (_p20) {
					return A3(_elm_lang$core$Time$spawnHelp, router, spawnList, existingDict);
				},
				killTask));
	});
var _elm_lang$core$Time$Every = F2(
	function (a, b) {
		return {ctor: 'Every', _0: a, _1: b};
	});
var _elm_lang$core$Time$every = F2(
	function (interval, tagger) {
		return _elm_lang$core$Time$subscription(
			A2(_elm_lang$core$Time$Every, interval, tagger));
	});
var _elm_lang$core$Time$subMap = F2(
	function (f, _p21) {
		var _p22 = _p21;
		return A2(
			_elm_lang$core$Time$Every,
			_p22._0,
			function (_p23) {
				return f(
					_p22._1(_p23));
			});
	});
_elm_lang$core$Native_Platform.effectManagers['Time'] = {pkg: 'elm-lang/core', init: _elm_lang$core$Time$init, onEffects: _elm_lang$core$Time$onEffects, onSelfMsg: _elm_lang$core$Time$onSelfMsg, tag: 'sub', subMap: _elm_lang$core$Time$subMap};

var _elm_community$elm_time$Time_DateTime$isValidTime = F4(
	function (hour, minute, second, millisecond) {
		return (_elm_lang$core$Native_Utils.cmp(hour, 0) > -1) && ((_elm_lang$core$Native_Utils.cmp(hour, 24) < 0) && ((_elm_lang$core$Native_Utils.cmp(minute, 0) > -1) && ((_elm_lang$core$Native_Utils.cmp(minute, 60) < 0) && ((_elm_lang$core$Native_Utils.cmp(second, 0) > -1) && ((_elm_lang$core$Native_Utils.cmp(second, 60) < 0) && ((_elm_lang$core$Native_Utils.cmp(millisecond, 0) > -1) && (_elm_lang$core$Native_Utils.cmp(millisecond, 1000) < 0)))))));
	});
var _elm_community$elm_time$Time_DateTime$delta = F2(
	function (_p1, _p0) {
		var _p2 = _p1;
		var _p6 = _p2._0;
		var _p3 = _p0;
		var _p5 = _p3._0;
		var _p4 = A2(_elm_community$elm_time$Time_Date$delta, _p6.date, _p5.date);
		var years = _p4.years;
		var months = _p4.months;
		var days = _p4.days;
		var milliseconds = (days * _elm_community$elm_time$Time_Internal$dayMs) + (_p6.offset - _p5.offset);
		var hours = (milliseconds / _elm_community$elm_time$Time_Internal$hourMs) | 0;
		var minutes = (milliseconds / _elm_community$elm_time$Time_Internal$minuteMs) | 0;
		var seconds = (milliseconds / _elm_community$elm_time$Time_Internal$secondMs) | 0;
		return {years: years, months: months, days: days, hours: hours, minutes: minutes, seconds: seconds, milliseconds: milliseconds};
	});
var _elm_community$elm_time$Time_DateTime$millisecond = function (_p7) {
	var _p8 = _p7;
	return A2(
		_elm_lang$core$Basics_ops['%'],
		A2(
			_elm_lang$core$Basics_ops['%'],
			A2(_elm_lang$core$Basics_ops['%'], _p8._0.offset, _elm_community$elm_time$Time_Internal$hourMs),
			_elm_community$elm_time$Time_Internal$minuteMs),
		_elm_community$elm_time$Time_Internal$secondMs);
};
var _elm_community$elm_time$Time_DateTime$second = function (_p9) {
	var _p10 = _p9;
	return (A2(
		_elm_lang$core$Basics_ops['%'],
		A2(_elm_lang$core$Basics_ops['%'], _p10._0.offset, _elm_community$elm_time$Time_Internal$hourMs),
		_elm_community$elm_time$Time_Internal$minuteMs) / _elm_community$elm_time$Time_Internal$secondMs) | 0;
};
var _elm_community$elm_time$Time_DateTime$minute = function (_p11) {
	var _p12 = _p11;
	return (A2(_elm_lang$core$Basics_ops['%'], _p12._0.offset, _elm_community$elm_time$Time_Internal$hourMs) / _elm_community$elm_time$Time_Internal$minuteMs) | 0;
};
var _elm_community$elm_time$Time_DateTime$hour = function (_p13) {
	var _p14 = _p13;
	return (_p14._0.offset / _elm_community$elm_time$Time_Internal$hourMs) | 0;
};
var _elm_community$elm_time$Time_DateTime$toTuple = function (_p15) {
	var _p16 = _p15;
	var _p18 = _p16;
	var _p17 = _elm_community$elm_time$Time_Date$toTuple(_p16._0.date);
	var year = _p17._0;
	var month = _p17._1;
	var day = _p17._2;
	return {
		ctor: '_Tuple7',
		_0: year,
		_1: month,
		_2: day,
		_3: _elm_community$elm_time$Time_DateTime$hour(_p18),
		_4: _elm_community$elm_time$Time_DateTime$minute(_p18),
		_5: _elm_community$elm_time$Time_DateTime$second(_p18),
		_6: _elm_community$elm_time$Time_DateTime$millisecond(_p18)
	};
};
var _elm_community$elm_time$Time_DateTime$weekday = function (_p19) {
	var _p20 = _p19;
	return _elm_community$elm_time$Time_Date$weekday(_p20._0.date);
};
var _elm_community$elm_time$Time_DateTime$day = function (_p21) {
	var _p22 = _p21;
	return _elm_community$elm_time$Time_Date$day(_p22._0.date);
};
var _elm_community$elm_time$Time_DateTime$month = function (_p23) {
	var _p24 = _p23;
	return _elm_community$elm_time$Time_Date$month(_p24._0.date);
};
var _elm_community$elm_time$Time_DateTime$year = function (_p25) {
	var _p26 = _p25;
	return _elm_community$elm_time$Time_Date$year(_p26._0.date);
};
var _elm_community$elm_time$Time_DateTime$toISO8601 = function (time) {
	return A2(
		_elm_lang$core$Basics_ops['++'],
		_elm_lang$core$Basics$toString(
			_elm_community$elm_time$Time_DateTime$year(time)),
		A2(
			_elm_lang$core$Basics_ops['++'],
			'-',
			A2(
				_elm_lang$core$Basics_ops['++'],
				_elm_community$elm_time$Time_Internal$padded(
					_elm_community$elm_time$Time_DateTime$month(time)),
				A2(
					_elm_lang$core$Basics_ops['++'],
					'-',
					A2(
						_elm_lang$core$Basics_ops['++'],
						_elm_community$elm_time$Time_Internal$padded(
							_elm_community$elm_time$Time_DateTime$day(time)),
						A2(
							_elm_lang$core$Basics_ops['++'],
							'T',
							A2(
								_elm_lang$core$Basics_ops['++'],
								_elm_community$elm_time$Time_Internal$padded(
									_elm_community$elm_time$Time_DateTime$hour(time)),
								A2(
									_elm_lang$core$Basics_ops['++'],
									':',
									A2(
										_elm_lang$core$Basics_ops['++'],
										_elm_community$elm_time$Time_Internal$padded(
											_elm_community$elm_time$Time_DateTime$minute(time)),
										A2(
											_elm_lang$core$Basics_ops['++'],
											':',
											A2(
												_elm_lang$core$Basics_ops['++'],
												_elm_community$elm_time$Time_Internal$padded(
													_elm_community$elm_time$Time_DateTime$second(time)),
												A2(
													_elm_lang$core$Basics_ops['++'],
													'.',
													A2(
														_elm_lang$core$Basics_ops['++'],
														_elm_community$elm_time$Time_Internal$padded3(
															_elm_community$elm_time$Time_DateTime$millisecond(time)),
														'Z')))))))))))));
};
var _elm_community$elm_time$Time_DateTime$compare = F2(
	function (dt1, dt2) {
		return A2(
			_elm_lang$core$Basics$compare,
			_elm_community$elm_time$Time_DateTime$toISO8601(dt1),
			_elm_community$elm_time$Time_DateTime$toISO8601(dt2));
	});
var _elm_community$elm_time$Time_DateTime$date = function (_p27) {
	var _p28 = _p27;
	return _p28._0.date;
};
var _elm_community$elm_time$Time_DateTime$zero = _elm_community$elm_time$Time_Internal$zero;
var _elm_community$elm_time$Time_DateTime$DateTimeDelta = F7(
	function (a, b, c, d, e, f, g) {
		return {years: a, months: b, days: c, hours: d, minutes: e, seconds: f, milliseconds: g};
	});
var _elm_community$elm_time$Time_DateTime$DateTime = function (a) {
	return {ctor: 'DateTime', _0: a};
};
var _elm_community$elm_time$Time_DateTime$dateTime = function (_p29) {
	var _p30 = _p29;
	return _elm_community$elm_time$Time_DateTime$DateTime(
		{
			date: A3(_elm_community$elm_time$Time_Date$date, _p30.year, _p30.month, _p30.day),
			offset: _elm_community$elm_time$Time_Internal$offsetFromTimeData(_p30)
		});
};
var _elm_community$elm_time$Time_DateTime$epoch = _elm_community$elm_time$Time_DateTime$dateTime(
	_elm_lang$core$Native_Utils.update(
		_elm_community$elm_time$Time_DateTime$zero,
		{year: 1970}));
var _elm_community$elm_time$Time_DateTime$toTimestamp = function (time) {
	return _elm_lang$core$Basics$toFloat(
		function (_) {
			return _.milliseconds;
		}(
			A2(_elm_community$elm_time$Time_DateTime$delta, time, _elm_community$elm_time$Time_DateTime$epoch)));
};
var _elm_community$elm_time$Time_DateTime$fromTuple = function (_p31) {
	var _p32 = _p31;
	return _elm_community$elm_time$Time_DateTime$dateTime(
		{year: _p32._0, month: _p32._1, day: _p32._2, hour: _p32._3, minute: _p32._4, second: _p32._5, millisecond: _p32._6});
};
var _elm_community$elm_time$Time_DateTime$mkDateTime = F2(
	function (date, time) {
		return _elm_community$elm_time$Time_DateTime$DateTime(
			{
				date: date,
				offset: _elm_community$elm_time$Time_Internal$offsetFromTimeData(time)
			});
	});
var _elm_community$elm_time$Time_DateTime$setHour = F2(
	function (hour, _p33) {
		var _p34 = _p33;
		var _p35 = _p34;
		return A2(
			_elm_community$elm_time$Time_DateTime$mkDateTime,
			_p34._0.date,
			{
				hour: hour,
				minute: _elm_community$elm_time$Time_DateTime$minute(_p35),
				second: _elm_community$elm_time$Time_DateTime$second(_p35),
				millisecond: _elm_community$elm_time$Time_DateTime$millisecond(_p35)
			});
	});
var _elm_community$elm_time$Time_DateTime$setMinute = F2(
	function (minute, _p36) {
		var _p37 = _p36;
		var _p38 = _p37;
		return A2(
			_elm_community$elm_time$Time_DateTime$mkDateTime,
			_p37._0.date,
			{
				hour: _elm_community$elm_time$Time_DateTime$hour(_p38),
				minute: minute,
				second: _elm_community$elm_time$Time_DateTime$second(_p38),
				millisecond: _elm_community$elm_time$Time_DateTime$millisecond(_p38)
			});
	});
var _elm_community$elm_time$Time_DateTime$setSecond = F2(
	function (second, _p39) {
		var _p40 = _p39;
		var _p41 = _p40;
		return A2(
			_elm_community$elm_time$Time_DateTime$mkDateTime,
			_p40._0.date,
			{
				hour: _elm_community$elm_time$Time_DateTime$hour(_p41),
				minute: _elm_community$elm_time$Time_DateTime$minute(_p41),
				second: second,
				millisecond: _elm_community$elm_time$Time_DateTime$millisecond(_p41)
			});
	});
var _elm_community$elm_time$Time_DateTime$setMillisecond = F2(
	function (millisecond, _p42) {
		var _p43 = _p42;
		var _p44 = _p43;
		return A2(
			_elm_community$elm_time$Time_DateTime$mkDateTime,
			_p43._0.date,
			{
				hour: _elm_community$elm_time$Time_DateTime$hour(_p44),
				minute: _elm_community$elm_time$Time_DateTime$minute(_p44),
				second: _elm_community$elm_time$Time_DateTime$second(_p44),
				millisecond: millisecond
			});
	});
var _elm_community$elm_time$Time_DateTime$setDate = F2(
	function (date, _p45) {
		var _p46 = _p45;
		return _elm_community$elm_time$Time_DateTime$DateTime(
			{date: date, offset: _p46._0.offset});
	});
var _elm_community$elm_time$Time_DateTime$setYear = F2(
	function (year, _p47) {
		var _p48 = _p47;
		return _elm_community$elm_time$Time_DateTime$DateTime(
			{
				date: A2(_elm_community$elm_time$Time_Date$setYear, year, _p48._0.date),
				offset: _p48._0.offset
			});
	});
var _elm_community$elm_time$Time_DateTime$setMonth = F2(
	function (month, _p49) {
		var _p50 = _p49;
		return _elm_community$elm_time$Time_DateTime$DateTime(
			{
				date: A2(_elm_community$elm_time$Time_Date$setMonth, month, _p50._0.date),
				offset: _p50._0.offset
			});
	});
var _elm_community$elm_time$Time_DateTime$setDay = F2(
	function (day, _p51) {
		var _p52 = _p51;
		return _elm_community$elm_time$Time_DateTime$DateTime(
			{
				date: A2(_elm_community$elm_time$Time_Date$setDay, day, _p52._0.date),
				offset: _p52._0.offset
			});
	});
var _elm_community$elm_time$Time_DateTime$addYears = F2(
	function (years, _p53) {
		var _p54 = _p53;
		return _elm_community$elm_time$Time_DateTime$DateTime(
			{
				date: A2(_elm_community$elm_time$Time_Date$addYears, years, _p54._0.date),
				offset: _p54._0.offset
			});
	});
var _elm_community$elm_time$Time_DateTime$addMonths = F2(
	function (months, _p55) {
		var _p56 = _p55;
		return _elm_community$elm_time$Time_DateTime$DateTime(
			{
				date: A2(_elm_community$elm_time$Time_Date$addMonths, months, _p56._0.date),
				offset: _p56._0.offset
			});
	});
var _elm_community$elm_time$Time_DateTime$addDays = F2(
	function (days, _p57) {
		var _p58 = _p57;
		return _elm_community$elm_time$Time_DateTime$DateTime(
			{
				date: A2(_elm_community$elm_time$Time_Date$addDays, days, _p58._0.date),
				offset: _p58._0.offset
			});
	});
var _elm_community$elm_time$Time_DateTime$addMilliseconds = F2(
	function (ms, _p59) {
		var _p60 = _p59;
		var total = ms + _p60._0.offset;
		var _p61 = function () {
			if (_elm_lang$core$Native_Utils.cmp(total, 0) < 0) {
				var offset = A2(_elm_lang$core$Basics$rem, total, _elm_community$elm_time$Time_Internal$dayMs);
				var days = 0 - (((_elm_lang$core$Basics$abs(total) / _elm_community$elm_time$Time_Internal$dayMs) | 0) + 1);
				return _elm_lang$core$Native_Utils.eq(offset, 0) ? {ctor: '_Tuple2', _0: days + 1, _1: 0} : {
					ctor: '_Tuple2',
					_0: days,
					_1: _elm_community$elm_time$Time_Internal$dayMs + A2(_elm_lang$core$Basics$rem, offset, _elm_community$elm_time$Time_Internal$dayMs)
				};
			} else {
				return {
					ctor: '_Tuple2',
					_0: (total / _elm_community$elm_time$Time_Internal$dayMs) | 0,
					_1: A2(_elm_lang$core$Basics$rem, total, _elm_community$elm_time$Time_Internal$dayMs)
				};
			}
		}();
		var days = _p61._0;
		var newOffset = _p61._1;
		return _elm_community$elm_time$Time_DateTime$DateTime(
			{
				date: A2(_elm_community$elm_time$Time_Date$addDays, days, _p60._0.date),
				offset: newOffset
			});
	});
var _elm_community$elm_time$Time_DateTime$addHours = F2(
	function (hours, time) {
		return A2(_elm_community$elm_time$Time_DateTime$addMilliseconds, hours * _elm_community$elm_time$Time_Internal$hourMs, time);
	});
var _elm_community$elm_time$Time_DateTime$addMinutes = F2(
	function (minutes, time) {
		return A2(_elm_community$elm_time$Time_DateTime$addMilliseconds, minutes * _elm_community$elm_time$Time_Internal$minuteMs, time);
	});
var _elm_community$elm_time$Time_DateTime$fromISO8601 = function (input) {
	var convert = function (_p62) {
		var _p63 = _p62;
		var _p69 = _p63._0._0;
		var _p68 = _p63._1._2;
		var _p67 = _p63._0._1;
		var _p66 = _p63._1._1;
		var _p65 = _p63._1._0;
		var _p64 = _p63._0._2;
		return (A3(_elm_community$elm_time$Time_Date$isValidDate, _p69, _p67, _p64) && A4(_elm_community$elm_time$Time_DateTime$isValidTime, _p65, _p66, _p68, 0)) ? _elm_community$parser_combinators$Combine$succeed(
			A2(
				_elm_community$elm_time$Time_DateTime$addMinutes,
				0 - _p63._2,
				_elm_community$elm_time$Time_DateTime$dateTime(
					A7(_elm_community$elm_time$Time_Internal$DateTimeData, _p69, _p67, _p64, _p65, _p66, _p68, _p63._1._3)))) : _elm_community$parser_combinators$Combine$fail('invalid date');
	};
	var offset = A2(
		_elm_community$parser_combinators$Combine_ops['<|>'],
		A2(
			_elm_community$parser_combinators$Combine_ops['<$'],
			0,
			_elm_community$parser_combinators$Combine$string('Z')),
		A2(
			_elm_community$parser_combinators$Combine_ops['<*>'],
			A2(
				_elm_community$parser_combinators$Combine_ops['<*>'],
				A2(
					_elm_community$parser_combinators$Combine_ops['<$>'],
					F3(
						function (s, h, m) {
							return ((s * h) * 60) + (s * m);
						}),
					_elm_community$parser_combinators$Combine$choice(
						{
							ctor: '::',
							_0: A2(
								_elm_community$parser_combinators$Combine_ops['<$'],
								1,
								_elm_community$parser_combinators$Combine$string('+')),
							_1: {
								ctor: '::',
								_0: A2(
									_elm_community$parser_combinators$Combine_ops['<$'],
									-1,
									_elm_community$parser_combinators$Combine$string('-')),
								_1: {
									ctor: '::',
									_0: A2(
										_elm_community$parser_combinators$Combine_ops['<$'],
										-1,
										_elm_community$parser_combinators$Combine$string('−')),
									_1: {ctor: '[]'}
								}
							}
						})),
				A3(_elm_community$elm_time$Time_Internal$digitsInRange, 2, 0, 23)),
			A2(
				_elm_community$parser_combinators$Combine_ops['*>'],
				A2(
					_elm_community$parser_combinators$Combine$optional,
					':',
					_elm_community$parser_combinators$Combine$string(':')),
				A3(_elm_community$elm_time$Time_Internal$digitsInRange, 2, 0, 59))));
	var basicDate = A2(
		_elm_community$parser_combinators$Combine_ops['<*>'],
		A2(
			_elm_community$parser_combinators$Combine_ops['<*>'],
			A2(
				_elm_community$parser_combinators$Combine_ops['<$>'],
				F3(
					function (v0, v1, v2) {
						return {ctor: '_Tuple3', _0: v0, _1: v1, _2: v2};
					}),
				A3(_elm_community$elm_time$Time_Internal$digitsInRange, 4, 0, 9999)),
			A3(_elm_community$elm_time$Time_Internal$digitsInRange, 2, 1, 12)),
		A3(_elm_community$elm_time$Time_Internal$digitsInRange, 2, 1, 31));
	var extendedDate = A2(
		_elm_community$parser_combinators$Combine_ops['<*>'],
		A2(
			_elm_community$parser_combinators$Combine_ops['<*>'],
			A2(
				_elm_community$parser_combinators$Combine_ops['<$>'],
				F3(
					function (v0, v1, v2) {
						return {ctor: '_Tuple3', _0: v0, _1: v1, _2: v2};
					}),
				_elm_community$parser_combinators$Combine_Num$int),
			A2(
				_elm_community$parser_combinators$Combine_ops['*>'],
				_elm_community$parser_combinators$Combine$string('-'),
				A2(_elm_community$elm_time$Time_Internal$intRange, 1, 12))),
		A2(
			_elm_community$parser_combinators$Combine_ops['*>'],
			_elm_community$parser_combinators$Combine$string('-'),
			A2(_elm_community$elm_time$Time_Internal$intRange, 1, 31)));
	var fraction = function () {
		var parseInteger = function (s) {
			var _p70 = _elm_lang$core$String$toInt(s);
			if (_p70.ctor === 'Err') {
				return 0;
			} else {
				return _p70._0;
			}
		};
		var keepUpTo3Places = function (fractionString) {
			var denominator = Math.pow(
				10,
				_elm_lang$core$String$length(fractionString));
			var numerator = parseInteger(fractionString);
			return _elm_lang$core$Basics$round(
				(_elm_community$elm_time$Time_Internal$secondMs * _elm_lang$core$Basics$toFloat(numerator)) / _elm_lang$core$Basics$toFloat(denominator));
		};
		var convert = function (fractionString) {
			return keepUpTo3Places(fractionString);
		};
		var getFractionString = A2(
			_elm_community$parser_combinators$Combine_ops['<$>'],
			function (p) {
				return p;
			},
			_elm_community$parser_combinators$Combine$regex('\\d*'));
		return A2(_elm_community$parser_combinators$Combine_ops['<$>'], convert, getFractionString);
	}();
	var extendedTime = A2(
		_elm_community$parser_combinators$Combine_ops['<*>'],
		A2(
			_elm_community$parser_combinators$Combine_ops['<*>'],
			A2(
				_elm_community$parser_combinators$Combine_ops['<*>'],
				A2(
					_elm_community$parser_combinators$Combine_ops['<$>'],
					F4(
						function (v0, v1, v2, v3) {
							return {ctor: '_Tuple4', _0: v0, _1: v1, _2: v2, _3: v3};
						}),
					A2(
						_elm_community$parser_combinators$Combine_ops['*>'],
						_elm_community$parser_combinators$Combine$string('T'),
						A2(_elm_community$elm_time$Time_Internal$intRange, 0, 23))),
				A2(
					_elm_community$parser_combinators$Combine_ops['*>'],
					_elm_community$parser_combinators$Combine$string(':'),
					A2(_elm_community$elm_time$Time_Internal$intRange, 0, 59))),
			A2(
				_elm_community$parser_combinators$Combine_ops['*>'],
				_elm_community$parser_combinators$Combine$string(':'),
				A2(_elm_community$elm_time$Time_Internal$intRange, 0, 59))),
		A2(
			_elm_community$parser_combinators$Combine$optional,
			0,
			A2(
				_elm_community$parser_combinators$Combine_ops['*>'],
				_elm_community$parser_combinators$Combine$regex('[,.]'),
				fraction)));
	var basicTime = A2(
		_elm_community$parser_combinators$Combine_ops['<*>'],
		A2(
			_elm_community$parser_combinators$Combine_ops['<*>'],
			A2(
				_elm_community$parser_combinators$Combine_ops['<*>'],
				A2(
					_elm_community$parser_combinators$Combine_ops['<$>'],
					F4(
						function (v0, v1, v2, v3) {
							return {ctor: '_Tuple4', _0: v0, _1: v1, _2: v2, _3: v3};
						}),
					A2(
						_elm_community$parser_combinators$Combine_ops['*>'],
						_elm_community$parser_combinators$Combine$string('T'),
						A3(_elm_community$elm_time$Time_Internal$digitsInRange, 2, 0, 23))),
				A3(_elm_community$elm_time$Time_Internal$digitsInRange, 2, 0, 59)),
			A3(_elm_community$elm_time$Time_Internal$digitsInRange, 2, 0, 59)),
		A2(
			_elm_community$parser_combinators$Combine$optional,
			0,
			A2(
				_elm_community$parser_combinators$Combine_ops['*>'],
				_elm_community$parser_combinators$Combine$regex('[,.]'),
				fraction)));
	var datetime = A2(
		_elm_community$parser_combinators$Combine_ops['<*'],
		A2(
			_elm_community$parser_combinators$Combine_ops['<*>'],
			A2(
				_elm_community$parser_combinators$Combine_ops['<*>'],
				A2(
					_elm_community$parser_combinators$Combine_ops['<$>'],
					F3(
						function (v0, v1, v2) {
							return {ctor: '_Tuple3', _0: v0, _1: v1, _2: v2};
						}),
					A2(_elm_community$parser_combinators$Combine_ops['<|>'], extendedDate, basicDate)),
				A2(_elm_community$parser_combinators$Combine_ops['<|>'], extendedTime, basicTime)),
			offset),
		_elm_community$parser_combinators$Combine$end);
	var _p71 = A2(
		_elm_community$parser_combinators$Combine$parse,
		A2(_elm_community$parser_combinators$Combine_ops['>>='], datetime, convert),
		input);
	if (_p71.ctor === 'Ok') {
		return _elm_lang$core$Result$Ok(_p71._0._2);
	} else {
		var messages = A2(_elm_lang$core$String$join, ' or ', _p71._0._2);
		return _elm_lang$core$Result$Err(
			A2(
				_elm_lang$core$Basics_ops['++'],
				'Errors encountered at position ',
				A2(
					_elm_lang$core$Basics_ops['++'],
					_elm_lang$core$Basics$toString(_p71._0._1.position),
					A2(_elm_lang$core$Basics_ops['++'], ': ', messages))));
	}
};
var _elm_community$elm_time$Time_DateTime$addSeconds = F2(
	function (seconds, time) {
		return A2(_elm_community$elm_time$Time_DateTime$addMilliseconds, seconds * _elm_community$elm_time$Time_Internal$secondMs, time);
	});
var _elm_community$elm_time$Time_DateTime$fromTimestamp = function (timestamp) {
	return A2(
		_elm_community$elm_time$Time_DateTime$addMilliseconds,
		_elm_lang$core$Basics$round(timestamp),
		_elm_community$elm_time$Time_DateTime$epoch);
};

var _elm_community$elm_time$Time_TimeZone_ops = _elm_community$elm_time$Time_TimeZone_ops || {};
_elm_community$elm_time$Time_TimeZone_ops['!!'] = F2(
	function (xs, i) {
		var _p0 = _elm_lang$core$List$head(
			A2(_elm_lang$core$List$drop, i, xs));
		if (_p0.ctor === 'Nothing') {
			return _elm_lang$core$Native_Utils.crashCase(
				'Time.TimeZone',
				{
					start: {line: 296, column: 5},
					end: {line: 301, column: 14}
				},
				_p0)(
				A2(
					_elm_lang$core$Basics_ops['++'],
					'index too large: xs=',
					A2(
						_elm_lang$core$Basics_ops['++'],
						_elm_lang$core$Basics$toString(xs),
						A2(
							_elm_lang$core$Basics_ops['++'],
							' i=',
							_elm_lang$core$Basics$toString(i)))));
		} else {
			return _p0._0;
		}
	});
var _elm_community$elm_time$Time_TimeZone$unsafeBase60 = F3(
	function (sign, whole, frac) {
		var toNum = function (c) {
			var n = _elm_lang$core$Basics$toFloat(
				_elm_lang$core$Char$toCode(c));
			return (_elm_lang$core$Native_Utils.cmp(n, 96) > 0) ? (n - 87) : ((_elm_lang$core$Native_Utils.cmp(n, 64) > 0) ? (n - 29) : (n - 48));
		};
		var toWhole = F2(
			function (cs, acc) {
				toWhole:
				while (true) {
					var _p2 = cs;
					if (_p2.ctor === '[]') {
						return acc;
					} else {
						var _v2 = _p2._1,
							_v3 = (60 * acc) + toNum(_p2._0);
						cs = _v2;
						acc = _v3;
						continue toWhole;
					}
				}
			});
		var toFrac = F3(
			function (cs, mul, acc) {
				toFrac:
				while (true) {
					var mul_ = mul / 60;
					var _p3 = cs;
					if (_p3.ctor === '[]') {
						return acc;
					} else {
						var _v5 = _p3._1,
							_v6 = mul_,
							_v7 = acc + (mul_ * toNum(_p3._0));
						cs = _v5;
						mul = _v6;
						acc = _v7;
						continue toFrac;
					}
				}
			});
		return A2(
			F2(
				function (x, y) {
					return x * y;
				}),
			_elm_lang$core$Basics$toFloat(sign),
			A3(
				toFrac,
				_elm_lang$core$String$toList(frac),
				1,
				A2(
					toWhole,
					_elm_lang$core$String$toList(whole),
					0)));
	});
var _elm_community$elm_time$Time_TimeZone$base60String = _elm_community$parser_combinators$Combine$regex('[0-9a-zA-Z]+');
var _elm_community$elm_time$Time_TimeZone$base60 = function () {
	var convert = function (_p4) {
		var _p5 = _p4;
		var _p7 = _p5._1;
		var _p6 = _p5._2;
		return (_elm_lang$core$Native_Utils.eq(_p7, '') && _elm_lang$core$Native_Utils.eq(_p6, '')) ? _elm_community$parser_combinators$Combine$fail('expected an alphanumeric character or .') : _elm_community$parser_combinators$Combine$succeed(
			A3(_elm_community$elm_time$Time_TimeZone$unsafeBase60, _p5._0, _p7, _p6));
	};
	var decode = A2(
		_elm_community$parser_combinators$Combine_ops['<*>'],
		A2(
			_elm_community$parser_combinators$Combine_ops['<*>'],
			A2(
				_elm_community$parser_combinators$Combine_ops['<$>'],
				F3(
					function (v0, v1, v2) {
						return {ctor: '_Tuple3', _0: v0, _1: v1, _2: v2};
					}),
				_elm_community$parser_combinators$Combine_Num$sign),
			A2(_elm_community$parser_combinators$Combine$optional, '', _elm_community$elm_time$Time_TimeZone$base60String)),
		A2(
			_elm_community$parser_combinators$Combine$optional,
			'',
			A2(
				_elm_community$parser_combinators$Combine_ops['*>'],
				_elm_community$parser_combinators$Combine$string('.'),
				_elm_community$elm_time$Time_TimeZone$base60String)));
	return A2(_elm_community$parser_combinators$Combine_ops['>>='], decode, convert);
}();
var _elm_community$elm_time$Time_TimeZone$name = function (_p8) {
	var _p9 = _p8;
	return _p9._0.name;
};
var _elm_community$elm_time$Time_TimeZone$find = F2(
	function (time, spans) {
		var go = function (xs) {
			go:
			while (true) {
				var _p10 = xs;
				if (_p10.ctor === '[]') {
					return _elm_lang$core$Native_Utils.crashCase(
						'Time.TimeZone',
						{
							start: {line: 100, column: 13},
							end: {line: 108, column: 30}
						},
						_p10)('find: invalid span list');
				} else {
					var _p12 = _p10._0;
					if ((_elm_lang$core$Native_Utils.cmp(time, _p12.from) > -1) && (_elm_lang$core$Native_Utils.cmp(time, _p12.until) < 0)) {
						return _p12;
					} else {
						var _v11 = _p10._1;
						xs = _v11;
						continue go;
					}
				}
			}
		};
		return go(spans);
	});
var _elm_community$elm_time$Time_TimeZone$offset = F2(
	function (time, _p13) {
		var _p14 = _p13;
		return function (_) {
			return _.offset;
		}(
			A2(_elm_community$elm_time$Time_TimeZone$find, time, _p14._0.spans));
	});
var _elm_community$elm_time$Time_TimeZone$offsetString = F2(
	function (time, timeZone) {
		var utcOffset = (A2(_elm_community$elm_time$Time_TimeZone$offset, time, timeZone) / _elm_community$elm_time$Time_Internal$minuteMs) | 0;
		var hours = (_elm_lang$core$Basics$abs(utcOffset) / 60) | 0;
		var minutes = A2(
			_elm_lang$core$Basics_ops['%'],
			_elm_lang$core$Basics$abs(utcOffset),
			60);
		var string = A2(
			_elm_lang$core$Basics_ops['++'],
			_elm_community$elm_time$Time_Internal$padded(hours),
			A2(
				_elm_lang$core$Basics_ops['++'],
				':',
				_elm_community$elm_time$Time_Internal$padded(minutes)));
		return (_elm_lang$core$Native_Utils.cmp(utcOffset, 0) < 1) ? A2(_elm_lang$core$Basics_ops['++'], '+', string) : A2(_elm_lang$core$Basics_ops['++'], '-', string);
	});
var _elm_community$elm_time$Time_TimeZone$abbreviation = F2(
	function (time, _p15) {
		var _p16 = _p15;
		return function (_) {
			return _.abbreviation;
		}(
			A2(_elm_community$elm_time$Time_TimeZone$find, time, _p16._0.spans));
	});
var _elm_community$elm_time$Time_TimeZone$Span = F4(
	function (a, b, c, d) {
		return {from: a, until: b, abbreviation: c, offset: d};
	});
var _elm_community$elm_time$Time_TimeZone$PackedTimeZone = F5(
	function (a, b, c, d, e) {
		return {name: a, abbrevs: b, offsets: c, indices: d, diffs: e};
	});
var _elm_community$elm_time$Time_TimeZone$TimeZone = function (a) {
	return {ctor: 'TimeZone', _0: a};
};
var _elm_community$elm_time$Time_TimeZone$setName = F2(
	function (name, _p17) {
		var _p18 = _p17;
		return _elm_community$elm_time$Time_TimeZone$TimeZone(
			_elm_lang$core$Native_Utils.update(
				_p18._0,
				{name: name}));
	});
var _elm_community$elm_time$Time_TimeZone$packedTimeZone = function () {
	var span = F4(
		function (times, data, i, idx) {
			return {
				from: A2(_elm_community$elm_time$Time_TimeZone_ops['!!'], times, i),
				until: A2(_elm_community$elm_time$Time_TimeZone_ops['!!'], times, i + 1),
				abbreviation: A2(_elm_community$elm_time$Time_TimeZone_ops['!!'], data.abbrevs, idx),
				offset: _elm_lang$core$Basics$round(
					A2(_elm_community$elm_time$Time_TimeZone_ops['!!'], data.offsets, idx) * _elm_community$elm_time$Time_Internal$minuteMs)
			};
		});
	var convert = function (data) {
		var times = (!_elm_lang$core$List$isEmpty(data.diffs)) ? A3(
			_elm_lang$core$List$scanl,
			F2(
				function (x, y) {
					return x + y;
				}),
			A2(_elm_community$elm_time$Time_TimeZone_ops['!!'], data.diffs, 0),
			A2(_elm_lang$core$List$drop, 1, data.diffs)) : {ctor: '[]'};
		var paddedTimes = A2(
			_elm_lang$core$Basics_ops['++'],
			{
				ctor: '::',
				_0: -1 / 0,
				_1: {ctor: '[]'}
			},
			A2(
				_elm_lang$core$Basics_ops['++'],
				times,
				{
					ctor: '::',
					_0: 1 / 0,
					_1: {ctor: '[]'}
				}));
		return _elm_community$elm_time$Time_TimeZone$TimeZone(
			{
				name: data.name,
				spans: A2(
					_elm_lang$core$List$indexedMap,
					A2(span, paddedTimes, data),
					data.indices)
			});
	};
	var validate = function (data) {
		var maxIndex = A2(
			_elm_lang$core$Maybe$withDefault,
			0,
			_elm_lang$core$List$maximum(data.indices));
		var offsets = _elm_lang$core$List$length(data.offsets);
		var abbrevs = _elm_lang$core$List$length(data.abbrevs);
		return (!_elm_lang$core$Native_Utils.eq(abbrevs, offsets)) ? _elm_community$parser_combinators$Combine$fail('abbrevs and offsets have different lengths') : ((_elm_lang$core$Native_Utils.cmp(maxIndex, abbrevs) > -1) ? _elm_community$parser_combinators$Combine$fail('highest index is longer than both abbrevs and offsets') : _elm_community$parser_combinators$Combine$succeed(data));
	};
	var diffs = A2(
		_elm_community$parser_combinators$Combine_ops['<$>'],
		_elm_lang$core$List$map(
			F2(
				function (x, y) {
					return x * y;
				})(60000)),
		A2(
			_elm_community$parser_combinators$Combine$sepBy,
			_elm_community$parser_combinators$Combine$string(' '),
			_elm_community$elm_time$Time_TimeZone$base60));
	var indices = A2(
		_elm_community$parser_combinators$Combine_ops['<*'],
		A2(
			_elm_community$parser_combinators$Combine_ops['<$>'],
			function (s) {
				return A2(
					_elm_lang$core$List$map,
					function (n) {
						return _elm_lang$core$Basics$floor(
							A3(_elm_community$elm_time$Time_TimeZone$unsafeBase60, 1, n, ''));
					},
					A2(_elm_lang$core$String$split, '', s));
			},
			_elm_community$parser_combinators$Combine$regex('[^|]+')),
		_elm_community$parser_combinators$Combine$string('|'));
	var offsets = A2(
		_elm_community$parser_combinators$Combine_ops['<*'],
		A2(
			_elm_community$parser_combinators$Combine$sepBy1,
			_elm_community$parser_combinators$Combine$string(' '),
			_elm_community$elm_time$Time_TimeZone$base60),
		_elm_community$parser_combinators$Combine$string('|'));
	var abbrevs = A2(
		_elm_community$parser_combinators$Combine_ops['<*'],
		A2(
			_elm_community$parser_combinators$Combine$sepBy1,
			_elm_community$parser_combinators$Combine$string(' '),
			_elm_community$parser_combinators$Combine$regex('[^ |]+')),
		_elm_community$parser_combinators$Combine$string('|'));
	var name = A2(
		_elm_community$parser_combinators$Combine_ops['<*'],
		_elm_community$parser_combinators$Combine$regex('[^|]+'),
		_elm_community$parser_combinators$Combine$string('|'));
	var decode = A2(
		_elm_community$parser_combinators$Combine_ops['<*>'],
		A2(
			_elm_community$parser_combinators$Combine_ops['<*>'],
			A2(
				_elm_community$parser_combinators$Combine_ops['<*>'],
				A2(
					_elm_community$parser_combinators$Combine_ops['<*>'],
					A2(_elm_community$parser_combinators$Combine_ops['<$>'], _elm_community$elm_time$Time_TimeZone$PackedTimeZone, name),
					abbrevs),
				offsets),
			indices),
		diffs);
	return A2(
		_elm_community$parser_combinators$Combine_ops['<$>'],
		convert,
		A2(_elm_community$parser_combinators$Combine_ops['>>='], decode, validate));
}();
var _elm_community$elm_time$Time_TimeZone$unpack = function (data) {
	var _p19 = A2(_elm_community$parser_combinators$Combine$parse, _elm_community$elm_time$Time_TimeZone$packedTimeZone, data);
	if (_p19.ctor === 'Ok') {
		return _elm_lang$core$Result$Ok(_p19._0._2);
	} else {
		return _elm_lang$core$Result$Err(_p19._0._2);
	}
};

var _elm_community$elm_time$Time_TimeZoneData$link = F2(
	function (link, lz) {
		return A2(
			_elm_lang$lazy$Lazy$map,
			_elm_community$elm_time$Time_TimeZone$setName(link),
			lz);
	});
var _elm_community$elm_time$Time_TimeZoneData$unpack = function (data) {
	var helper = function (_p0) {
		var _p1 = _p0;
		var _p2 = _elm_community$elm_time$Time_TimeZone$unpack(data);
		if (_p2.ctor === 'Err') {
			var messages = A2(_elm_lang$core$String$join, ' or ', _p2._0);
			return _elm_lang$core$Native_Utils.crash(
				'Time.TimeZoneData',
				{
					start: {line: 18, column: 25},
					end: {line: 18, column: 36}
				})(
				A2(
					_elm_lang$core$Basics_ops['++'],
					'failed to parse zone \'',
					A2(
						_elm_lang$core$Basics_ops['++'],
						data,
						A2(_elm_lang$core$Basics_ops['++'], '\': ', messages))));
		} else {
			return _p2._0;
		}
	};
	return _elm_lang$lazy$Lazy$lazy(helper);
};
var _elm_community$elm_time$Time_TimeZoneData$africa_abidjan_l = _elm_community$elm_time$Time_TimeZoneData$unpack('Africa/Abidjan|LMT GMT|g.8 0|01|-2ldXH.Q|48e5');
var _elm_community$elm_time$Time_TimeZoneData$africa_accra_l = _elm_community$elm_time$Time_TimeZoneData$unpack('Africa/Accra|LMT GMT GHST|.Q 0 -k|012121212121212121212121212121212121212121212121|-26BbX.8 6tzX.8 MnE 1BAk MnE 1BAk MnE 1BAk MnE 1C0k MnE 1BAk MnE 1BAk MnE 1BAk MnE 1C0k MnE 1BAk MnE 1BAk MnE 1BAk MnE 1C0k MnE 1BAk MnE 1BAk MnE 1BAk MnE 1C0k MnE 1BAk MnE 1BAk MnE 1BAk MnE 1C0k MnE 1BAk MnE 1BAk MnE|41e5');
var _elm_community$elm_time$Time_TimeZoneData$africa_algiers_l = _elm_community$elm_time$Time_TimeZoneData$unpack('Africa/Algiers|PMT WET WEST CET CEST|-9.l 0 -10 -10 -20|0121212121212121343431312123431213|-2nco9.l cNb9.l HA0 19A0 1iM0 11c0 1oo0 Wo0 1rc0 QM0 1EM0 UM0 DA0 Imo0 rd0 De0 9Xz0 1fb0 1ap0 16K0 2yo0 mEp0 hwL0 jxA0 11A0 dDd0 17b0 11B0 1cN0 2Dy0 1cN0 1fB0 1cL0|26e5');
var _elm_community$elm_time$Time_TimeZoneData$africa_bissau_l = _elm_community$elm_time$Time_TimeZoneData$unpack('Africa/Bissau|LMT WAT GMT|12.k 10 0|012|-2ldWV.E 2xonV.E|39e4');
var _elm_community$elm_time$Time_TimeZoneData$africa_cairo_l = _elm_community$elm_time$Time_TimeZoneData$unpack('Africa/Cairo|EET EEST|-20 -30|0101010101010101010101010101010101010101010101010101010101010101010101010101010101010101010101010101010101010101010101010101010|-1bIO0 vb0 1ip0 11z0 1iN0 1nz0 12p0 1pz0 10N0 1pz0 16p0 1jz0 s3d0 Vz0 1oN0 11b0 1oO0 10N0 1pz0 10N0 1pb0 10N0 1pb0 10N0 1pb0 10N0 1pz0 10N0 1pb0 10N0 1pb0 11d0 1oL0 11d0 1pb0 11d0 1oL0 11d0 1oL0 11d0 1oL0 11d0 1pb0 11d0 1oL0 11d0 1oL0 11d0 1oL0 11d0 1pb0 11d0 1oL0 11d0 1oL0 11d0 1oL0 11d0 1pb0 11d0 1oL0 11d0 1WL0 rd0 1Rz0 wp0 1pb0 11d0 1oL0 11d0 1oL0 11d0 1oL0 11d0 1pb0 11d0 1qL0 Xd0 1oL0 11d0 1oL0 11d0 1pb0 11d0 1oL0 11d0 1oL0 11d0 1ny0 11z0 1o10 11z0 1o10 11z0 1o10 11z0 1qN0 11z0 1o10 11z0 1o10 11z0 1o10 11z0 1o10 11z0 1qN0 11z0 1o10 11z0 1o10 WL0 1qN0 Rb0 1wp0 On0 1zd0 Lz0 1EN0 Fb0 c10 8n0 8Nd0 gL0 e10 mn0|15e6');
var _elm_community$elm_time$Time_TimeZoneData$africa_casablanca_l = _elm_community$elm_time$Time_TimeZoneData$unpack('Africa/Casablanca|LMT WET WEST CET|u.k 0 -10 -10|0121212121212121213121212121212121212121212121212121212121212121212121212121212121212121212121212121|-2gMnt.E 130Lt.E rb0 Dd0 dVb0 b6p0 TX0 EoB0 LL0 gnd0 rz0 43d0 AL0 1Nd0 XX0 1Cp0 pz0 dEp0 4mn0 SyN0 AL0 1Nd0 wn0 1FB0 Db0 1zd0 Lz0 1Nf0 wM0 co0 go0 1o00 s00 dA0 vc0 11A0 A00 e00 y00 11A0 uM0 e00 Dc0 11A0 s00 e00 IM0 WM0 mo0 gM0 LA0 WM0 jA0 e00 Rc0 11A0 e00 e00 U00 11A0 8o0 e00 11A0 11A0 5A0 e00 17c0 1fA0 1a00 1a00 1fA0 17c0 1io0 14o0 1lc0 11A0 1o00 11A0 1qM0 WM0 1qM0 WM0 1qM0 11A0 1o00 11A0 1o00 11A0 1qM0 WM0 1qM0 WM0 1qM0 WM0 1qM0 11A0 1lc0 14o0 1fA0|32e5');
var _elm_community$elm_time$Time_TimeZoneData$africa_ceuta_l = _elm_community$elm_time$Time_TimeZoneData$unpack('Africa/Ceuta|WET WEST CET CEST|0 -10 -10 -20|010101010101010101010232323232323232323232323232323232323232323232323232323232323232323232323232323232323232323232323232323232|-25KN0 11z0 drd0 18o0 3I00 17c0 1fA0 1a00 1io0 1a00 1y7p0 LL0 gnd0 rz0 43d0 AL0 1Nd0 XX0 1Cp0 pz0 dEp0 4VB0 1cM0 1cM0 1cM0 1cM0 1cM0 1cM0 1cM0 1cM0 1fA0 1cM0 1cM0 1cM0 1cM0 1cM0 1cM0 1cM0 1cM0 1cM0 1cM0 1fA0 1o00 11A0 1o00 11A0 1o00 11A0 1qM0 WM0 1qM0 WM0 1qM0 11A0 1o00 11A0 1o00 11A0 1qM0 WM0 1qM0 WM0 1qM0 WM0 1qM0 11A0 1o00 11A0 1o00 11A0 1qM0 WM0 1qM0 WM0 1qM0 11A0 1o00 11A0 1o00 11A0 1o00 11A0 1qM0 WM0 1qM0 WM0 1qM0 11A0 1o00 11A0 1o00 11A0 1qM0 WM0 1qM0 WM0 1qM0 11A0 1o00 11A0 1o00 11A0 1o00 11A0 1qM0 WM0 1qM0 WM0 1qM0 11A0 1o00 11A0 1o00 11A0 1qM0 WM0 1qM0 WM0 1qM0 WM0 1qM0 11A0 1o00 11A0 1o00|85e3');
var _elm_community$elm_time$Time_TimeZoneData$africa_el_aaiun_l = _elm_community$elm_time$Time_TimeZoneData$unpack('Africa/El_Aaiun|LMT WAT WET WEST|Q.M 10 0 -10|01232323232323232323232323232323232323232323232323232323232323232323232323232323232323232|-1rDz7.c 1GVA7.c 6L0 AL0 1Nd0 XX0 1Cp0 pz0 1cBB0 AL0 1Nd0 wn0 1FB0 Db0 1zd0 Lz0 1Nf0 wM0 co0 go0 1o00 s00 dA0 vc0 11A0 A00 e00 y00 11A0 uM0 e00 Dc0 11A0 s00 e00 IM0 WM0 mo0 gM0 LA0 WM0 jA0 e00 Rc0 11A0 e00 e00 U00 11A0 8o0 e00 11A0 11A0 5A0 e00 17c0 1fA0 1a00 1a00 1fA0 17c0 1io0 14o0 1lc0 11A0 1o00 11A0 1qM0 WM0 1qM0 WM0 1qM0 11A0 1o00 11A0 1o00 11A0 1qM0 WM0 1qM0 WM0 1qM0 WM0 1qM0 11A0 1lc0 14o0 1fA0|20e4');
var _elm_community$elm_time$Time_TimeZoneData$africa_johannesburg_l = _elm_community$elm_time$Time_TimeZoneData$unpack('Africa/Johannesburg|SAST SAST SAST|-1u -20 -30|012121|-2GJdu 1Ajdu 1cL0 1cN0 1cL0|84e5');
var _elm_community$elm_time$Time_TimeZoneData$africa_khartoum_l = _elm_community$elm_time$Time_TimeZoneData$unpack('Africa/Khartoum|LMT CAT CAST EAT|-2a.8 -20 -30 -30|01212121212121212121212121212121213|-1yW2a.8 1zK0a.8 16L0 1iN0 17b0 1jd0 17b0 1ip0 17z0 1i10 17X0 1hB0 18n0 1hd0 19b0 1gp0 19z0 1iN0 17b0 1ip0 17z0 1i10 18n0 1hd0 18L0 1gN0 19b0 1gp0 19z0 1iN0 17z0 1i10 17X0 yGd0|51e5');
var _elm_community$elm_time$Time_TimeZoneData$africa_lagos_l = _elm_community$elm_time$Time_TimeZoneData$unpack('Africa/Lagos|LMT WAT|-d.A -10|01|-22y0d.A|17e6');
var _elm_community$elm_time$Time_TimeZoneData$africa_maputo_l = _elm_community$elm_time$Time_TimeZoneData$unpack('Africa/Maputo|LMT CAT|-2a.k -20|01|-2GJea.k|26e5');
var _elm_community$elm_time$Time_TimeZoneData$africa_monrovia_l = _elm_community$elm_time$Time_TimeZoneData$unpack('Africa/Monrovia|MMT LRT GMT|H.8 I.u 0|012|-23Lzg.Q 29s01.m|11e5');
var _elm_community$elm_time$Time_TimeZoneData$africa_nairobi_l = _elm_community$elm_time$Time_TimeZoneData$unpack('Africa/Nairobi|LMT EAT BEAT BEAUT|-2r.g -30 -2u -2J|01231|-1F3Cr.g 3Dzr.g okMu MFXJ|47e5');
var _elm_community$elm_time$Time_TimeZoneData$africa_ndjamena_l = _elm_community$elm_time$Time_TimeZoneData$unpack('Africa/Ndjamena|LMT WAT WAST|-10.c -10 -20|0121|-2le10.c 2J3c0.c Wn0|13e5');
var _elm_community$elm_time$Time_TimeZoneData$africa_tripoli_l = _elm_community$elm_time$Time_TimeZoneData$unpack('Africa/Tripoli|LMT CET CEST EET|-Q.I -10 -20 -20|012121213121212121212121213123123|-21JcQ.I 1hnBQ.I vx0 4iP0 xx0 4eN0 Bb0 7ip0 U0n0 A10 1db0 1cN0 1db0 1dd0 1db0 1eN0 1bb0 1e10 1cL0 1c10 1db0 1dd0 1db0 1cN0 1db0 1q10 fAn0 1ep0 1db0 AKq0 TA0 1o00|11e5');
var _elm_community$elm_time$Time_TimeZoneData$africa_tunis_l = _elm_community$elm_time$Time_TimeZoneData$unpack('Africa/Tunis|PMT CET CEST|-9.l -10 -20|0121212121212121212121212121212121|-2nco9.l 18pa9.l 1qM0 DA0 3Tc0 11B0 1ze0 WM0 7z0 3d0 14L0 1cN0 1f90 1ar0 16J0 1gXB0 WM0 1rA0 11c0 nwo0 Ko0 1cM0 1cM0 1rA0 10M0 zuM0 10N0 1aN0 1qM0 WM0 1qM0 11A0 1o00|20e5');
var _elm_community$elm_time$Time_TimeZoneData$africa_windhoek_l = _elm_community$elm_time$Time_TimeZoneData$unpack('Africa/Windhoek|SWAT SAST SAST CAT WAT WAST|-1u -20 -30 -20 -10 -20|012134545454545454545454545454545454545454545454545454545454545454545454545454545454545454545|-2GJdu 1Ajdu 1cL0 1SqL0 9NA0 11D0 1nX0 11B0 1qL0 WN0 1qL0 11B0 1nX0 11B0 1nX0 11B0 1nX0 11B0 1nX0 11B0 1qL0 WN0 1qL0 11B0 1nX0 11B0 1nX0 11B0 1nX0 11B0 1nX0 11B0 1qL0 11B0 1nX0 11B0 1nX0 11B0 1nX0 11B0 1nX0 11B0 1qL0 WN0 1qL0 11B0 1nX0 11B0 1nX0 11B0 1nX0 11B0 1nX0 11B0 1qL0 WN0 1qL0 11B0 1nX0 11B0 1nX0 11B0 1nX0 11B0 1qL0 WN0 1qL0 11B0 1nX0 11B0 1nX0 11B0 1nX0 11B0 1nX0 11B0 1qL0 WN0 1qL0 11B0 1nX0 11B0 1nX0 11B0 1nX0 11B0 1nX0 11B0 1qL0 11B0 1nX0 11B0|32e4');
var _elm_community$elm_time$Time_TimeZoneData$america_adak_l = _elm_community$elm_time$Time_TimeZoneData$unpack('America/Adak|NST NWT NPT BST BDT AHST HST HDT|b0 a0 a0 b0 a0 a0 a0 90|012034343434343434343434343434343456767676767676767676767676767676767676767676767676767676767676767676767676767676767676767676767676767676767676|-17SX0 8wW0 iB0 Qlb0 52O0 1cL0 1cN0 1cL0 1cN0 1fz0 1cN0 1cL0 1cN0 1cL0 s10 1Vz0 LB0 1BX0 1cN0 1fz0 1a10 1fz0 1cN0 1cL0 1cN0 1cL0 1cN0 1cL0 1cN0 1cL0 1cN0 1fz0 1a10 1fz0 cm0 10q0 1cL0 1cN0 1cL0 1cN0 1cL0 14p0 1lb0 14p0 1nX0 11B0 1nX0 11B0 1nX0 14p0 1lb0 14p0 1lb0 14p0 1nX0 11B0 1nX0 11B0 1nX0 14p0 1lb0 14p0 1lb0 14p0 1lb0 14p0 1nX0 11B0 1nX0 11B0 1nX0 14p0 1lb0 14p0 1lb0 14p0 1nX0 11B0 1nX0 11B0 1nX0 Rd0 1zb0 Op0 1zb0 Op0 1zb0 Rd0 1zb0 Op0 1zb0 Op0 1zb0 Op0 1zb0 Op0 1zb0 Op0 1zb0 Rd0 1zb0 Op0 1zb0 Op0 1zb0 Op0 1zb0 Op0 1zb0 Rd0 1zb0 Op0 1zb0 Op0 1zb0 Op0 1zb0 Op0 1zb0 Op0 1zb0 Rd0 1zb0 Op0 1zb0 Op0 1zb0 Op0 1zb0 Op0 1zb0 Rd0 1zb0 Op0 1zb0 Op0 1zb0 Op0 1zb0 Op0 1zb0 Op0 1zb0|326');
var _elm_community$elm_time$Time_TimeZoneData$america_anchorage_l = _elm_community$elm_time$Time_TimeZoneData$unpack('America/Anchorage|CAT CAWT CAPT AHST AHDT YST AKST AKDT|a0 90 90 a0 90 90 90 80|012034343434343434343434343434343456767676767676767676767676767676767676767676767676767676767676767676767676767676767676767676767676767676767676|-17T00 8wX0 iA0 Qlb0 52O0 1cL0 1cN0 1cL0 1cN0 1fz0 1cN0 1cL0 1cN0 1cL0 s10 1Vz0 LB0 1BX0 1cN0 1fz0 1a10 1fz0 1cN0 1cL0 1cN0 1cL0 1cN0 1cL0 1cN0 1cL0 1cN0 1fz0 1a10 1fz0 cm0 10q0 1cL0 1cN0 1cL0 1cN0 1cL0 14p0 1lb0 14p0 1nX0 11B0 1nX0 11B0 1nX0 14p0 1lb0 14p0 1lb0 14p0 1nX0 11B0 1nX0 11B0 1nX0 14p0 1lb0 14p0 1lb0 14p0 1lb0 14p0 1nX0 11B0 1nX0 11B0 1nX0 14p0 1lb0 14p0 1lb0 14p0 1nX0 11B0 1nX0 11B0 1nX0 Rd0 1zb0 Op0 1zb0 Op0 1zb0 Rd0 1zb0 Op0 1zb0 Op0 1zb0 Op0 1zb0 Op0 1zb0 Op0 1zb0 Rd0 1zb0 Op0 1zb0 Op0 1zb0 Op0 1zb0 Op0 1zb0 Rd0 1zb0 Op0 1zb0 Op0 1zb0 Op0 1zb0 Op0 1zb0 Op0 1zb0 Rd0 1zb0 Op0 1zb0 Op0 1zb0 Op0 1zb0 Op0 1zb0 Rd0 1zb0 Op0 1zb0 Op0 1zb0 Op0 1zb0 Op0 1zb0 Op0 1zb0|30e4');
var _elm_community$elm_time$Time_TimeZoneData$america_araguaina_l = _elm_community$elm_time$Time_TimeZoneData$unpack('America/Araguaina|LMT BRT BRST|3c.M 30 20|0121212121212121212121212121212121212121212121212121|-2glwL.c HdKL.c 1cc0 1e10 1bX0 Ezd0 So0 1vA0 Mn0 1BB0 ML0 1BB0 zX0 qe10 xb0 2ep0 nz0 1C10 zX0 1C10 LX0 1C10 Mn0 H210 Rb0 1tB0 IL0 1Fd0 FX0 1EN0 FX0 1HB0 Lz0 dMN0 Lz0 1zd0 Rb0 1wN0 Wn0 1tB0 Rb0 1tB0 WL0 1tB0 Rb0 1zd0 On0 1HB0 FX0 ny10 Lz0|14e4');
var _elm_community$elm_time$Time_TimeZoneData$america_argentina_buenos_aires_l = _elm_community$elm_time$Time_TimeZoneData$unpack('America/Argentina/Buenos_Aires|CMT ART ARST ART ARST|4g.M 40 30 30 20|0121212121212121212121212121212121212121213434343434343234343|-20UHH.c pKnH.c Mn0 1iN0 Tb0 1C10 LX0 1C10 LX0 1C10 LX0 1C10 Mn0 1C10 LX0 1C10 LX0 1C10 LX0 1C10 Mn0 MN0 2jz0 MN0 4lX0 u10 5Lb0 1pB0 Fnz0 u10 uL0 1vd0 SL0 1vd0 SL0 1vd0 17z0 1cN0 1fz0 1cN0 1cL0 1cN0 asn0 Db0 zvd0 Bz0 1tB0 TX0 1wp0 Rb0 1wp0 Rb0 1wp0 TX0 g0p0 10M0 j3c0 uL0 1qN0 WL0');
var _elm_community$elm_time$Time_TimeZoneData$america_argentina_catamarca_l = _elm_community$elm_time$Time_TimeZoneData$unpack('America/Argentina/Catamarca|CMT ART ARST ART ARST WART|4g.M 40 30 30 20 40|0121212121212121212121212121212121212121213434343454343235343|-20UHH.c pKnH.c Mn0 1iN0 Tb0 1C10 LX0 1C10 LX0 1C10 LX0 1C10 Mn0 1C10 LX0 1C10 LX0 1C10 LX0 1C10 Mn0 MN0 2jz0 MN0 4lX0 u10 5Lb0 1pB0 Fnz0 u10 uL0 1vd0 SL0 1vd0 SL0 1vd0 17z0 1cN0 1fz0 1cN0 1cL0 1cN0 asn0 Db0 zvd0 Bz0 1tB0 TX0 1wp0 Rb0 1wq0 Ra0 1wp0 TX0 g0p0 10M0 ako0 7B0 8zb0 uL0');
var _elm_community$elm_time$Time_TimeZoneData$america_argentina_cordoba_l = _elm_community$elm_time$Time_TimeZoneData$unpack('America/Argentina/Cordoba|CMT ART ARST ART ARST WART|4g.M 40 30 30 20 40|0121212121212121212121212121212121212121213434343454343234343|-20UHH.c pKnH.c Mn0 1iN0 Tb0 1C10 LX0 1C10 LX0 1C10 LX0 1C10 Mn0 1C10 LX0 1C10 LX0 1C10 LX0 1C10 Mn0 MN0 2jz0 MN0 4lX0 u10 5Lb0 1pB0 Fnz0 u10 uL0 1vd0 SL0 1vd0 SL0 1vd0 17z0 1cN0 1fz0 1cN0 1cL0 1cN0 asn0 Db0 zvd0 Bz0 1tB0 TX0 1wp0 Rb0 1wq0 Ra0 1wp0 TX0 g0p0 10M0 j3c0 uL0 1qN0 WL0');
var _elm_community$elm_time$Time_TimeZoneData$america_argentina_jujuy_l = _elm_community$elm_time$Time_TimeZoneData$unpack('America/Argentina/Jujuy|CMT ART ARST ART ARST WART WARST|4g.M 40 30 30 20 40 30|01212121212121212121212121212121212121212134343456543432343|-20UHH.c pKnH.c Mn0 1iN0 Tb0 1C10 LX0 1C10 LX0 1C10 LX0 1C10 Mn0 1C10 LX0 1C10 LX0 1C10 LX0 1C10 Mn0 MN0 2jz0 MN0 4lX0 u10 5Lb0 1pB0 Fnz0 u10 uL0 1vd0 SL0 1vd0 SL0 1vd0 17z0 1cN0 1fz0 1cN0 1cL0 1cN0 asn0 Db0 zvd0 Bz0 1tB0 TX0 1ze0 TX0 1ld0 WK0 1wp0 TX0 g0p0 10M0 j3c0 uL0');
var _elm_community$elm_time$Time_TimeZoneData$america_argentina_la_rioja_l = _elm_community$elm_time$Time_TimeZoneData$unpack('America/Argentina/La_Rioja|CMT ART ARST ART ARST WART|4g.M 40 30 30 20 40|01212121212121212121212121212121212121212134343434534343235343|-20UHH.c pKnH.c Mn0 1iN0 Tb0 1C10 LX0 1C10 LX0 1C10 LX0 1C10 Mn0 1C10 LX0 1C10 LX0 1C10 LX0 1C10 Mn0 MN0 2jz0 MN0 4lX0 u10 5Lb0 1pB0 Fnz0 u10 uL0 1vd0 SL0 1vd0 SL0 1vd0 17z0 1cN0 1fz0 1cN0 1cL0 1cN0 asn0 Db0 zvd0 Bz0 1tB0 TX0 1wp0 Qn0 qO0 16n0 Rb0 1wp0 TX0 g0p0 10M0 ako0 7B0 8zb0 uL0');
var _elm_community$elm_time$Time_TimeZoneData$america_argentina_mendoza_l = _elm_community$elm_time$Time_TimeZoneData$unpack('America/Argentina/Mendoza|CMT ART ARST ART ARST WART WARST|4g.M 40 30 30 20 40 30|0121212121212121212121212121212121212121213434345656543235343|-20UHH.c pKnH.c Mn0 1iN0 Tb0 1C10 LX0 1C10 LX0 1C10 LX0 1C10 Mn0 1C10 LX0 1C10 LX0 1C10 LX0 1C10 Mn0 MN0 2jz0 MN0 4lX0 u10 5Lb0 1pB0 Fnz0 u10 uL0 1vd0 SL0 1vd0 SL0 1vd0 17z0 1cN0 1fz0 1cN0 1cL0 1cN0 asn0 Db0 zvd0 Bz0 1tB0 TX0 1u20 SL0 1vd0 Tb0 1wp0 TW0 g0p0 10M0 agM0 Op0 7TX0 uL0');
var _elm_community$elm_time$Time_TimeZoneData$america_argentina_rio_gallegos_l = _elm_community$elm_time$Time_TimeZoneData$unpack('America/Argentina/Rio_Gallegos|CMT ART ARST ART ARST WART|4g.M 40 30 30 20 40|0121212121212121212121212121212121212121213434343434343235343|-20UHH.c pKnH.c Mn0 1iN0 Tb0 1C10 LX0 1C10 LX0 1C10 LX0 1C10 Mn0 1C10 LX0 1C10 LX0 1C10 LX0 1C10 Mn0 MN0 2jz0 MN0 4lX0 u10 5Lb0 1pB0 Fnz0 u10 uL0 1vd0 SL0 1vd0 SL0 1vd0 17z0 1cN0 1fz0 1cN0 1cL0 1cN0 asn0 Db0 zvd0 Bz0 1tB0 TX0 1wp0 Rb0 1wp0 Rb0 1wp0 TX0 g0p0 10M0 ako0 7B0 8zb0 uL0');
var _elm_community$elm_time$Time_TimeZoneData$america_argentina_salta_l = _elm_community$elm_time$Time_TimeZoneData$unpack('America/Argentina/Salta|CMT ART ARST ART ARST WART|4g.M 40 30 30 20 40|01212121212121212121212121212121212121212134343434543432343|-20UHH.c pKnH.c Mn0 1iN0 Tb0 1C10 LX0 1C10 LX0 1C10 LX0 1C10 Mn0 1C10 LX0 1C10 LX0 1C10 LX0 1C10 Mn0 MN0 2jz0 MN0 4lX0 u10 5Lb0 1pB0 Fnz0 u10 uL0 1vd0 SL0 1vd0 SL0 1vd0 17z0 1cN0 1fz0 1cN0 1cL0 1cN0 asn0 Db0 zvd0 Bz0 1tB0 TX0 1wp0 Rb0 1wq0 Ra0 1wp0 TX0 g0p0 10M0 j3c0 uL0');
var _elm_community$elm_time$Time_TimeZoneData$america_argentina_san_juan_l = _elm_community$elm_time$Time_TimeZoneData$unpack('America/Argentina/San_Juan|CMT ART ARST ART ARST WART|4g.M 40 30 30 20 40|01212121212121212121212121212121212121212134343434534343235343|-20UHH.c pKnH.c Mn0 1iN0 Tb0 1C10 LX0 1C10 LX0 1C10 LX0 1C10 Mn0 1C10 LX0 1C10 LX0 1C10 LX0 1C10 Mn0 MN0 2jz0 MN0 4lX0 u10 5Lb0 1pB0 Fnz0 u10 uL0 1vd0 SL0 1vd0 SL0 1vd0 17z0 1cN0 1fz0 1cN0 1cL0 1cN0 asn0 Db0 zvd0 Bz0 1tB0 TX0 1wp0 Qn0 qO0 16n0 Rb0 1wp0 TX0 g0p0 10M0 ak00 m10 8lb0 uL0');
var _elm_community$elm_time$Time_TimeZoneData$america_argentina_san_luis_l = _elm_community$elm_time$Time_TimeZoneData$unpack('America/Argentina/San_Luis|CMT ART ARST ART ARST WART WARST|4g.M 40 30 30 20 40 30|01212121212121212121212121212121212121212134343456536353465653|-20UHH.c pKnH.c Mn0 1iN0 Tb0 1C10 LX0 1C10 LX0 1C10 LX0 1C10 Mn0 1C10 LX0 1C10 LX0 1C10 LX0 1C10 Mn0 MN0 2jz0 MN0 4lX0 u10 5Lb0 1pB0 Fnz0 u10 uL0 1vd0 SL0 1vd0 SL0 1vd0 17z0 1cN0 1fz0 1cN0 1cL0 1cN0 asn0 Db0 zvd0 Bz0 1tB0 XX0 1q20 SL0 AN0 kin0 10M0 ak00 m10 8lb0 8L0 jd0 1qN0 WL0 1qN0');
var _elm_community$elm_time$Time_TimeZoneData$america_argentina_tucuman_l = _elm_community$elm_time$Time_TimeZoneData$unpack('America/Argentina/Tucuman|CMT ART ARST ART ARST WART|4g.M 40 30 30 20 40|012121212121212121212121212121212121212121343434345434323534343|-20UHH.c pKnH.c Mn0 1iN0 Tb0 1C10 LX0 1C10 LX0 1C10 LX0 1C10 Mn0 1C10 LX0 1C10 LX0 1C10 LX0 1C10 Mn0 MN0 2jz0 MN0 4lX0 u10 5Lb0 1pB0 Fnz0 u10 uL0 1vd0 SL0 1vd0 SL0 1vd0 17z0 1cN0 1fz0 1cN0 1cL0 1cN0 asn0 Db0 zvd0 Bz0 1tB0 TX0 1wp0 Rb0 1wq0 Ra0 1wp0 TX0 g0p0 10M0 ako0 4N0 8BX0 uL0 1qN0 WL0');
var _elm_community$elm_time$Time_TimeZoneData$america_argentina_ushuaia_l = _elm_community$elm_time$Time_TimeZoneData$unpack('America/Argentina/Ushuaia|CMT ART ARST ART ARST WART|4g.M 40 30 30 20 40|0121212121212121212121212121212121212121213434343434343235343|-20UHH.c pKnH.c Mn0 1iN0 Tb0 1C10 LX0 1C10 LX0 1C10 LX0 1C10 Mn0 1C10 LX0 1C10 LX0 1C10 LX0 1C10 Mn0 MN0 2jz0 MN0 4lX0 u10 5Lb0 1pB0 Fnz0 u10 uL0 1vd0 SL0 1vd0 SL0 1vd0 17z0 1cN0 1fz0 1cN0 1cL0 1cN0 asn0 Db0 zvd0 Bz0 1tB0 TX0 1wp0 Rb0 1wp0 Rb0 1wp0 TX0 g0p0 10M0 ajA0 8p0 8zb0 uL0');
var _elm_community$elm_time$Time_TimeZoneData$america_asuncion_l = _elm_community$elm_time$Time_TimeZoneData$unpack('America/Asuncion|AMT PYT PYT PYST|3O.E 40 30 30|012131313131313131313131313131313131313131313131313131313131313131313131313131313131313131313131313131313131313131313131313131313|-1x589.k 1DKM9.k 3CL0 3Dd0 10L0 1pB0 10n0 1pB0 10n0 1pB0 1cL0 1dd0 1db0 1dd0 1cL0 1dd0 1cL0 1dd0 1cL0 1dd0 1db0 1dd0 1cL0 1dd0 1cL0 1dd0 1cL0 1dd0 1db0 1dd0 1cL0 1lB0 14n0 1dd0 1cL0 1fd0 WL0 1rd0 1aL0 1dB0 Xz0 1qp0 Xb0 1qN0 10L0 1rB0 TX0 1tB0 WL0 1qN0 11z0 1o10 11z0 1o10 11z0 1qN0 1cL0 WN0 1qL0 11B0 1nX0 1ip0 WL0 1qN0 WL0 1qN0 WL0 1tB0 TX0 1tB0 TX0 1tB0 19X0 1a10 1fz0 1a10 1fz0 1cN0 17b0 1ip0 17b0 1ip0 17b0 1ip0 19X0 1fB0 19X0 1fB0 19X0 1ip0 17b0 1ip0 17b0 1ip0 19X0 1fB0 19X0 1fB0 19X0 1fB0 19X0 1ip0 17b0 1ip0 17b0 1ip0 19X0 1fB0 19X0 1fB0 19X0 1ip0 17b0 1ip0 17b0 1ip0 19X0 1fB0 19X0 1fB0 19X0 1fB0 19X0 1ip0 17b0 1ip0 17b0 1ip0|28e5');
var _elm_community$elm_time$Time_TimeZoneData$america_atikokan_l = _elm_community$elm_time$Time_TimeZoneData$unpack('America/Atikokan|CST CDT CWT CPT EST|60 50 50 50 50|0101234|-25TQ0 1in0 Rnb0 3je0 8x30 iw0|28e2');
var _elm_community$elm_time$Time_TimeZoneData$america_bahia_banderas_l = _elm_community$elm_time$Time_TimeZoneData$unpack('America/Bahia_Banderas|LMT MST CST PST MDT CDT|71 70 60 80 60 50|0121212131414141414141414141414141414152525252525252525252525252525252525252525252525252525252|-1UQF0 deL0 8lc0 17c0 10M0 1dd0 otX0 gmN0 P2N0 13Vd0 1lb0 14p0 1lb0 14p0 1lb0 14p0 1nX0 11B0 1nX0 1fB0 WL0 1fB0 1lb0 14p0 1lb0 14p0 1nX0 11B0 1nX0 11B0 1nX0 11B0 1nX0 14p0 1lb0 14p0 1lb0 14p0 1nW0 11B0 1nX0 11B0 1nX0 14p0 1lb0 14p0 1lb0 14p0 1lb0 14p0 1nX0 11B0 1nX0 11B0 1nX0 14p0 1lb0 14p0 1lb0 14p0 1nX0 11B0 1nX0 11B0 1nX0 14p0 1lb0 14p0 1lb0 14p0 1lb0 14p0 1nX0 11B0 1nX0 11B0 1nX0 14p0 1lb0 14p0 1lb0 14p0 1nX0 11B0 1nX0 11B0 1nX0 11B0 1nX0 14p0 1lb0 14p0 1lb0|84e3');
var _elm_community$elm_time$Time_TimeZoneData$america_bahia_l = _elm_community$elm_time$Time_TimeZoneData$unpack('America/Bahia|LMT BRT BRST|2y.4 30 20|01212121212121212121212121212121212121212121212121212121212121|-2glxp.U HdLp.U 1cc0 1e10 1bX0 Ezd0 So0 1vA0 Mn0 1BB0 ML0 1BB0 zX0 qe10 xb0 2ep0 nz0 1C10 zX0 1C10 LX0 1C10 Mn0 H210 Rb0 1tB0 IL0 1Fd0 FX0 1EN0 FX0 1HB0 Lz0 1EN0 Lz0 1C10 IL0 1HB0 Db0 1HB0 On0 1zd0 On0 1zd0 Lz0 1zd0 Rb0 1wN0 Wn0 1tB0 Rb0 1tB0 WL0 1tB0 Rb0 1zd0 On0 1HB0 FX0 l5B0 Rb0|27e5');
var _elm_community$elm_time$Time_TimeZoneData$america_barbados_l = _elm_community$elm_time$Time_TimeZoneData$unpack('America/Barbados|LMT BMT AST ADT|3W.t 3W.t 40 30|01232323232|-1Q0I1.v jsM0 1ODC1.v IL0 1ip0 17b0 1ip0 17b0 1ld0 13b0|28e4');
var _elm_community$elm_time$Time_TimeZoneData$america_belem_l = _elm_community$elm_time$Time_TimeZoneData$unpack('America/Belem|LMT BRT BRST|3d.U 30 20|012121212121212121212121212121|-2glwK.4 HdKK.4 1cc0 1e10 1bX0 Ezd0 So0 1vA0 Mn0 1BB0 ML0 1BB0 zX0 qe10 xb0 2ep0 nz0 1C10 zX0 1C10 LX0 1C10 Mn0 H210 Rb0 1tB0 IL0 1Fd0 FX0|20e5');
var _elm_community$elm_time$Time_TimeZoneData$america_belize_l = _elm_community$elm_time$Time_TimeZoneData$unpack('America/Belize|LMT CST CHDT CDT|5Q.M 60 5u 50|01212121212121212121212121212121212121212121212121213131|-2kBu7.c fPA7.c Onu 1zcu Rbu 1wou Rbu 1wou Rbu 1zcu Onu 1zcu Onu 1zcu Rbu 1wou Rbu 1wou Rbu 1wou Rbu 1zcu Onu 1zcu Onu 1zcu Rbu 1wou Rbu 1wou Rbu 1zcu Onu 1zcu Onu 1zcu Onu 1zcu Rbu 1wou Rbu 1wou Rbu 1zcu Onu 1zcu Onu 1zcu Rbu 1wou Rbu 1f0Mu qn0 lxB0 mn0|57e3');
var _elm_community$elm_time$Time_TimeZoneData$america_blanc_sablon_l = _elm_community$elm_time$Time_TimeZoneData$unpack('America/Blanc-Sablon|AST ADT AWT APT|40 30 30 30|010230|-25TS0 1in0 UGp0 8x50 iu0|11e2');
var _elm_community$elm_time$Time_TimeZoneData$america_boa_vista_l = _elm_community$elm_time$Time_TimeZoneData$unpack('America/Boa_Vista|LMT AMT AMST|42.E 40 30|0121212121212121212121212121212121|-2glvV.k HdKV.k 1cc0 1e10 1bX0 Ezd0 So0 1vA0 Mn0 1BB0 ML0 1BB0 zX0 qe10 xb0 2ep0 nz0 1C10 zX0 1C10 LX0 1C10 Mn0 H210 Rb0 1tB0 IL0 1Fd0 FX0 smp0 WL0 1tB0 2L0|62e2');
var _elm_community$elm_time$Time_TimeZoneData$america_bogota_l = _elm_community$elm_time$Time_TimeZoneData$unpack('America/Bogota|BMT COT COST|4U.g 50 40|0121|-2eb73.I 38yo3.I 2en0|90e5');
var _elm_community$elm_time$Time_TimeZoneData$america_boise_l = _elm_community$elm_time$Time_TimeZoneData$unpack('America/Boise|PST PDT MST MWT MPT MDT|80 70 70 60 60 60|0101023425252525252525252525252525252525252525252525252525252525252525252525252525252525252525252525252525252525252525252525252525252525252525252525252|-261q0 1nX0 11B0 1nX0 8C10 JCL0 8x20 ix0 QwN0 1cL0 1cN0 1cL0 1cN0 1cL0 1cN0 1cL0 1cN0 1fz0 1cN0 1cL0 1cN0 1cL0 Dd0 1Kn0 LB0 1BX0 1cN0 1fz0 1a10 1fz0 1cN0 1cL0 1cN0 1cL0 1cN0 1cL0 1cN0 1cL0 1cN0 1fz0 1a10 1fz0 1cN0 1cL0 1cN0 1cL0 1cN0 1cL0 14p0 1lb0 14p0 1nX0 11B0 1nX0 11B0 1nX0 14p0 1lb0 14p0 1lb0 14p0 1nX0 11B0 1nX0 11B0 1nX0 14p0 1lb0 14p0 1lb0 14p0 1lb0 14p0 1nX0 11B0 1nX0 11B0 1nX0 14p0 1lb0 14p0 1lb0 14p0 1nX0 11B0 1nX0 11B0 1nX0 Rd0 1zb0 Op0 1zb0 Op0 1zb0 Rd0 1zb0 Op0 1zb0 Op0 1zb0 Op0 1zb0 Op0 1zb0 Op0 1zb0 Rd0 1zb0 Op0 1zb0 Op0 1zb0 Op0 1zb0 Op0 1zb0 Rd0 1zb0 Op0 1zb0 Op0 1zb0 Op0 1zb0 Op0 1zb0 Op0 1zb0 Rd0 1zb0 Op0 1zb0 Op0 1zb0 Op0 1zb0 Op0 1zb0 Rd0 1zb0 Op0 1zb0 Op0 1zb0 Op0 1zb0 Op0 1zb0 Op0 1zb0|21e4');
var _elm_community$elm_time$Time_TimeZoneData$america_cambridge_bay_l = _elm_community$elm_time$Time_TimeZoneData$unpack('America/Cambridge_Bay|-00 MST MWT MPT MDDT MDT CST CDT EST|0 70 60 60 50 60 60 50 50|0123141515151515151515151515151515151515151515678651515151515151515151515151515151515151515151515151515151515151515151515151|-21Jc0 RO90 8x20 ix0 LCL0 1fA0 zgO0 1cL0 1cN0 1cL0 1cN0 1fz0 1a10 1fz0 1cN0 1cL0 1cN0 1cL0 1cN0 1cL0 14p0 1lb0 14p0 1nX0 11B0 1nX0 11B0 1nX0 14p0 1lb0 14p0 1lb0 14p0 1nX0 11B0 1nX0 11B0 1nX0 14p0 1lb0 14p0 1lb0 14p0 1lb0 14p0 1nX0 11A0 1nX0 2K0 WQ0 1nX0 14p0 1lb0 14p0 1lb0 14p0 1nX0 11B0 1nX0 11B0 1nX0 Rd0 1zb0 Op0 1zb0 Op0 1zb0 Rd0 1zb0 Op0 1zb0 Op0 1zb0 Op0 1zb0 Op0 1zb0 Op0 1zb0 Rd0 1zb0 Op0 1zb0 Op0 1zb0 Op0 1zb0 Op0 1zb0 Rd0 1zb0 Op0 1zb0 Op0 1zb0 Op0 1zb0 Op0 1zb0 Op0 1zb0 Rd0 1zb0 Op0 1zb0 Op0 1zb0 Op0 1zb0 Op0 1zb0 Rd0 1zb0 Op0 1zb0 Op0 1zb0 Op0 1zb0 Op0 1zb0 Op0 1zb0|15e2');
var _elm_community$elm_time$Time_TimeZoneData$america_campo_grande_l = _elm_community$elm_time$Time_TimeZoneData$unpack('America/Campo_Grande|LMT AMT AMST|3C.s 40 30|012121212121212121212121212121212121212121212121212121212121212121212121212121212121212121212121212121212121212121212121212121212|-2glwl.w HdLl.w 1cc0 1e10 1bX0 Ezd0 So0 1vA0 Mn0 1BB0 ML0 1BB0 zX0 qe10 xb0 2ep0 nz0 1C10 zX0 1C10 LX0 1C10 Mn0 H210 Rb0 1tB0 IL0 1Fd0 FX0 1EN0 FX0 1HB0 Lz0 1EN0 Lz0 1C10 IL0 1HB0 Db0 1HB0 On0 1zd0 On0 1zd0 Lz0 1zd0 Rb0 1wN0 Wn0 1tB0 Rb0 1tB0 WL0 1tB0 Rb0 1zd0 On0 1HB0 FX0 1C10 Lz0 1Ip0 HX0 1zd0 On0 1HB0 IL0 1wp0 On0 1C10 Lz0 1C10 On0 1zd0 On0 1zd0 Rb0 1zd0 Lz0 1C10 Lz0 1C10 On0 1zd0 On0 1zd0 On0 1zd0 On0 1C10 Lz0 1C10 Lz0 1C10 On0 1zd0 On0 1zd0 Rb0 1wp0 On0 1C10 Lz0 1C10 On0 1zd0 On0 1zd0 On0 1zd0 On0 1C10 Lz0 1C10 Lz0 1C10 Lz0 1C10 On0 1zd0 Rb0 1wp0 On0 1C10 Lz0 1C10 On0 1zd0|77e4');
var _elm_community$elm_time$Time_TimeZoneData$america_cancun_l = _elm_community$elm_time$Time_TimeZoneData$unpack('America/Cancun|LMT CST EST EDT CDT|5L.4 60 50 40 50|0123232341414141414141414141414141414141412|-1UQG0 2q2o0 yLB0 1lb0 14p0 1lb0 14p0 Lz0 xB0 14p0 1nX0 11B0 1nX0 1fB0 WL0 1fB0 1lb0 14p0 1lb0 14p0 1nX0 11B0 1nX0 11B0 1nX0 11B0 1nX0 14p0 1lb0 14p0 1lb0 14p0 1nX0 11B0 1nX0 11B0 1nX0 14p0 1lb0 14p0 1lb0 Dd0|63e4');
var _elm_community$elm_time$Time_TimeZoneData$america_caracas_l = _elm_community$elm_time$Time_TimeZoneData$unpack('America/Caracas|CMT VET VET|4r.E 4u 40|01212|-2kV7w.k 28KM2.k 1IwOu kqo0|29e5');
var _elm_community$elm_time$Time_TimeZoneData$america_cayenne_l = _elm_community$elm_time$Time_TimeZoneData$unpack('America/Cayenne|LMT GFT GFT|3t.k 40 30|012|-2mrwu.E 2gWou.E|58e3');
var _elm_community$elm_time$Time_TimeZoneData$america_chicago_l = _elm_community$elm_time$Time_TimeZoneData$unpack('America/Chicago|CST CDT EST CWT CPT|60 50 50 50 50|01010101010101010101010101010101010102010101010103401010101010101010101010101010101010101010101010101010101010101010101010101010101010101010101010101010101010101010101010101010101010101010101010101010101010101010101010101010101010101010|-261s0 1nX0 11B0 1nX0 1wp0 TX0 WN0 1qL0 1cN0 WL0 1qN0 11z0 1o10 11z0 1o10 11z0 1o10 11z0 1o10 11z0 1qN0 11z0 1o10 11z0 1o10 11z0 1o10 11z0 1o10 11z0 1qN0 WL0 1qN0 11z0 1o10 11z0 11B0 1Hz0 14p0 11z0 1o10 11z0 1qN0 WL0 1qN0 11z0 1o10 11z0 RB0 8x30 iw0 1o10 11z0 1o10 11z0 1o10 11z0 1o10 11z0 1qN0 WL0 1qN0 11z0 1o10 11z0 1o10 11z0 1o10 11z0 1o10 1fz0 1cN0 1cL0 1cN0 1cL0 1cN0 1cL0 1cN0 1cL0 1cN0 1fz0 1cN0 1cL0 1cN0 1cL0 1cN0 1cL0 1cN0 1cL0 1cN0 1fz0 1a10 1fz0 1cN0 1cL0 1cN0 1cL0 1cN0 1cL0 1cN0 1cL0 1cN0 1fz0 1cN0 1cL0 1cN0 1cL0 s10 1Vz0 LB0 1BX0 1cN0 1fz0 1a10 1fz0 1cN0 1cL0 1cN0 1cL0 1cN0 1cL0 1cN0 1cL0 1cN0 1fz0 1a10 1fz0 1cN0 1cL0 1cN0 1cL0 1cN0 1cL0 14p0 1lb0 14p0 1nX0 11B0 1nX0 11B0 1nX0 14p0 1lb0 14p0 1lb0 14p0 1nX0 11B0 1nX0 11B0 1nX0 14p0 1lb0 14p0 1lb0 14p0 1lb0 14p0 1nX0 11B0 1nX0 11B0 1nX0 14p0 1lb0 14p0 1lb0 14p0 1nX0 11B0 1nX0 11B0 1nX0 Rd0 1zb0 Op0 1zb0 Op0 1zb0 Rd0 1zb0 Op0 1zb0 Op0 1zb0 Op0 1zb0 Op0 1zb0 Op0 1zb0 Rd0 1zb0 Op0 1zb0 Op0 1zb0 Op0 1zb0 Op0 1zb0 Rd0 1zb0 Op0 1zb0 Op0 1zb0 Op0 1zb0 Op0 1zb0 Op0 1zb0 Rd0 1zb0 Op0 1zb0 Op0 1zb0 Op0 1zb0 Op0 1zb0 Rd0 1zb0 Op0 1zb0 Op0 1zb0 Op0 1zb0 Op0 1zb0 Op0 1zb0|92e5');
var _elm_community$elm_time$Time_TimeZoneData$america_chihuahua_l = _elm_community$elm_time$Time_TimeZoneData$unpack('America/Chihuahua|LMT MST CST CDT MDT|74.k 70 60 50 60|0121212323241414141414141414141414141414141414141414141414141414141414141414141414141414141|-1UQF0 deL0 8lc0 17c0 10M0 1dd0 2zQN0 1lb0 14p0 1lb0 14q0 1lb0 14p0 1nX0 11B0 1nX0 1fB0 WL0 1fB0 1lb0 14p0 1lb0 14p0 1nX0 11B0 1nX0 11B0 1nX0 11B0 1nX0 14p0 1lb0 14p0 1lb0 14p0 1nX0 11B0 1nX0 11B0 1nX0 14p0 1lb0 14p0 1lb0 14p0 1lb0 14p0 1nX0 11B0 1nX0 11B0 1nX0 14p0 1lb0 14p0 1lb0 14p0 1nX0 11B0 1nX0 11B0 1nX0 14p0 1lb0 14p0 1lb0 14p0 1lb0 14p0 1nX0 11B0 1nX0 11B0 1nX0 14p0 1lb0 14p0 1lb0 14p0 1nX0 11B0 1nX0 11B0 1nX0 11B0 1nX0 14p0 1lb0 14p0 1lb0|81e4');
var _elm_community$elm_time$Time_TimeZoneData$america_costa_rica_l = _elm_community$elm_time$Time_TimeZoneData$unpack('America/Costa_Rica|SJMT CST CDT|5A.d 60 50|0121212121|-1Xd6n.L 2lu0n.L Db0 1Kp0 Db0 pRB0 15b0 1kp0 mL0|12e5');
var _elm_community$elm_time$Time_TimeZoneData$america_creston_l = _elm_community$elm_time$Time_TimeZoneData$unpack('America/Creston|MST PST|70 80|010|-29DR0 43B0|53e2');
var _elm_community$elm_time$Time_TimeZoneData$america_cuiaba_l = _elm_community$elm_time$Time_TimeZoneData$unpack('America/Cuiaba|LMT AMT AMST|3I.k 40 30|0121212121212121212121212121212121212121212121212121212121212121212121212121212121212121212121212121212121212121212121212121212|-2glwf.E HdLf.E 1cc0 1e10 1bX0 Ezd0 So0 1vA0 Mn0 1BB0 ML0 1BB0 zX0 qe10 xb0 2ep0 nz0 1C10 zX0 1C10 LX0 1C10 Mn0 H210 Rb0 1tB0 IL0 1Fd0 FX0 1EN0 FX0 1HB0 Lz0 1EN0 Lz0 1C10 IL0 1HB0 Db0 1HB0 On0 1zd0 On0 1zd0 Lz0 1zd0 Rb0 1wN0 Wn0 1tB0 Rb0 1tB0 WL0 1tB0 Rb0 1zd0 On0 1HB0 FX0 4a10 HX0 1zd0 On0 1HB0 IL0 1wp0 On0 1C10 Lz0 1C10 On0 1zd0 On0 1zd0 Rb0 1zd0 Lz0 1C10 Lz0 1C10 On0 1zd0 On0 1zd0 On0 1zd0 On0 1C10 Lz0 1C10 Lz0 1C10 On0 1zd0 On0 1zd0 Rb0 1wp0 On0 1C10 Lz0 1C10 On0 1zd0 On0 1zd0 On0 1zd0 On0 1C10 Lz0 1C10 Lz0 1C10 Lz0 1C10 On0 1zd0 Rb0 1wp0 On0 1C10 Lz0 1C10 On0 1zd0|54e4');
var _elm_community$elm_time$Time_TimeZoneData$america_curacao_l = _elm_community$elm_time$Time_TimeZoneData$unpack('America/Curacao|LMT ANT AST|4z.L 4u 40|012|-2kV7o.d 28KLS.d|15e4');
var _elm_community$elm_time$Time_TimeZoneData$america_danmarkshavn_l = _elm_community$elm_time$Time_TimeZoneData$unpack('America/Danmarkshavn|LMT WGT WGST GMT|1e.E 30 20 0|01212121212121212121212121212121213|-2a5WJ.k 2z5fJ.k 19U0 1cM0 1cM0 1cM0 1cM0 1cM0 1cM0 1cM0 1fA0 1cM0 1cM0 1cM0 1cM0 1cM0 1cM0 1cM0 1cM0 1cM0 1cM0 1cM0 1fA0 1cM0 1cM0 1cM0 1cM0 1cM0 1cM0 1cM0 1cM0 1cM0 1cM0 DC0|8');
var _elm_community$elm_time$Time_TimeZoneData$america_dawson_creek_l = _elm_community$elm_time$Time_TimeZoneData$unpack('America/Dawson_Creek|PST PDT PWT PPT MST|80 70 70 70 70|0102301010101010101010101010101010101010101010101010101014|-25TO0 1in0 UGp0 8x10 iy0 3NB0 11z0 1o10 11z0 1o10 11z0 1qN0 WL0 1qN0 11z0 1o10 11z0 1o10 11z0 1o10 11z0 1o10 11z0 1qN0 11z0 1o10 11z0 1o10 11z0 1o10 11z0 1o10 11z0 1qN0 WL0 1qN0 1cL0 1cN0 1cL0 1cN0 1cL0 1cN0 1fz0 1a10 1fz0 1cN0 1cL0 1cN0 1cL0 1cN0 1cL0 1cN0 1cL0 1cN0 1fz0 1cN0 ML0|12e3');
var _elm_community$elm_time$Time_TimeZoneData$america_dawson_l = _elm_community$elm_time$Time_TimeZoneData$unpack('America/Dawson|YST YDT YWT YPT YDDT PST PDT|90 80 80 80 70 80 70|0101023040565656565656565656565656565656565656565656565656565656565656565656565656565656565656565656565656565656565656565656565|-25TN0 1in0 1o10 13V0 Ser0 8x00 iz0 LCL0 1fA0 jrA0 fNd0 1cL0 1cN0 1cL0 1cN0 1fz0 1a10 1fz0 1cN0 1cL0 1cN0 1cL0 1cN0 1cL0 14p0 1lb0 14p0 1nX0 11B0 1nX0 11B0 1nX0 14p0 1lb0 14p0 1lb0 14p0 1nX0 11B0 1nX0 11B0 1nX0 14p0 1lb0 14p0 1lb0 14p0 1lb0 14p0 1nX0 11B0 1nX0 11B0 1nX0 14p0 1lb0 14p0 1lb0 14p0 1nX0 11B0 1nX0 11B0 1nX0 Rd0 1zb0 Op0 1zb0 Op0 1zb0 Rd0 1zb0 Op0 1zb0 Op0 1zb0 Op0 1zb0 Op0 1zb0 Op0 1zb0 Rd0 1zb0 Op0 1zb0 Op0 1zb0 Op0 1zb0 Op0 1zb0 Rd0 1zb0 Op0 1zb0 Op0 1zb0 Op0 1zb0 Op0 1zb0 Op0 1zb0 Rd0 1zb0 Op0 1zb0 Op0 1zb0 Op0 1zb0 Op0 1zb0 Rd0 1zb0 Op0 1zb0 Op0 1zb0 Op0 1zb0 Op0 1zb0 Op0 1zb0|13e2');
var _elm_community$elm_time$Time_TimeZoneData$america_denver_l = _elm_community$elm_time$Time_TimeZoneData$unpack('America/Denver|MST MDT MWT MPT|70 60 60 60|01010101023010101010101010101010101010101010101010101010101010101010101010101010101010101010101010101010101010101010101010101010101010101010101010101010101010|-261r0 1nX0 11B0 1nX0 11B0 1qL0 WN0 mn0 Ord0 8x20 ix0 LCN0 1fz0 1a10 1fz0 1cN0 1cL0 1cN0 1cL0 1cN0 1cL0 1cN0 1cL0 1cN0 1fz0 1cN0 1cL0 1cN0 1cL0 s10 1Vz0 LB0 1BX0 1cN0 1fz0 1a10 1fz0 1cN0 1cL0 1cN0 1cL0 1cN0 1cL0 1cN0 1cL0 1cN0 1fz0 1a10 1fz0 1cN0 1cL0 1cN0 1cL0 1cN0 1cL0 14p0 1lb0 14p0 1nX0 11B0 1nX0 11B0 1nX0 14p0 1lb0 14p0 1lb0 14p0 1nX0 11B0 1nX0 11B0 1nX0 14p0 1lb0 14p0 1lb0 14p0 1lb0 14p0 1nX0 11B0 1nX0 11B0 1nX0 14p0 1lb0 14p0 1lb0 14p0 1nX0 11B0 1nX0 11B0 1nX0 Rd0 1zb0 Op0 1zb0 Op0 1zb0 Rd0 1zb0 Op0 1zb0 Op0 1zb0 Op0 1zb0 Op0 1zb0 Op0 1zb0 Rd0 1zb0 Op0 1zb0 Op0 1zb0 Op0 1zb0 Op0 1zb0 Rd0 1zb0 Op0 1zb0 Op0 1zb0 Op0 1zb0 Op0 1zb0 Op0 1zb0 Rd0 1zb0 Op0 1zb0 Op0 1zb0 Op0 1zb0 Op0 1zb0 Rd0 1zb0 Op0 1zb0 Op0 1zb0 Op0 1zb0 Op0 1zb0 Op0 1zb0|26e5');
var _elm_community$elm_time$Time_TimeZoneData$america_detroit_l = _elm_community$elm_time$Time_TimeZoneData$unpack('America/Detroit|LMT CST EST EWT EPT EDT|5w.b 60 50 40 40 40|01234252525252525252525252525252525252525252525252525252525252525252525252525252525252525252525252525252525252525252525252525252525252525252|-2Cgir.N peqr.N 156L0 8x40 iv0 6fd0 11z0 Jy10 SL0 dnB0 1cL0 s10 1Vz0 1cN0 1cL0 1cN0 1fz0 1a10 1fz0 1cN0 1cL0 1cN0 1cL0 1cN0 1cL0 1cN0 1cL0 1cN0 1fz0 1a10 1fz0 1cN0 1cL0 1cN0 1cL0 1cN0 1cL0 14p0 1lb0 14p0 1nX0 11B0 1nX0 11B0 1nX0 14p0 1lb0 14p0 1lb0 14p0 1nX0 11B0 1nX0 11B0 1nX0 14p0 1lb0 14p0 1lb0 14p0 1lb0 14p0 1nX0 11B0 1nX0 11B0 1nX0 14p0 1lb0 14p0 1lb0 14p0 1nX0 11B0 1nX0 11B0 1nX0 Rd0 1zb0 Op0 1zb0 Op0 1zb0 Rd0 1zb0 Op0 1zb0 Op0 1zb0 Op0 1zb0 Op0 1zb0 Op0 1zb0 Rd0 1zb0 Op0 1zb0 Op0 1zb0 Op0 1zb0 Op0 1zb0 Rd0 1zb0 Op0 1zb0 Op0 1zb0 Op0 1zb0 Op0 1zb0 Op0 1zb0 Rd0 1zb0 Op0 1zb0 Op0 1zb0 Op0 1zb0 Op0 1zb0 Rd0 1zb0 Op0 1zb0 Op0 1zb0 Op0 1zb0 Op0 1zb0 Op0 1zb0|37e5');
var _elm_community$elm_time$Time_TimeZoneData$america_edmonton_l = _elm_community$elm_time$Time_TimeZoneData$unpack('America/Edmonton|LMT MST MDT MWT MPT|7x.Q 70 60 60 60|01212121212121341212121212121212121212121212121212121212121212121212121212121212121212121212121212121212121212121212121212121212121212121212121212121212121|-2yd4q.8 shdq.8 1in0 17d0 hz0 2dB0 1fz0 1a10 11z0 1qN0 WL0 1qN0 11z0 IGN0 8x20 ix0 3NB0 11z0 LFB0 1cL0 3Cp0 1cL0 66N0 1cL0 1cN0 1cL0 1cN0 1cL0 1cN0 1cL0 1cN0 1fz0 1a10 1fz0 1cN0 1cL0 1cN0 1cL0 1cN0 1cL0 1cN0 1cL0 1cN0 1fz0 1a10 1fz0 1cN0 1cL0 1cN0 1cL0 1cN0 1cL0 14p0 1lb0 14p0 1nX0 11B0 1nX0 11B0 1nX0 14p0 1lb0 14p0 1lb0 14p0 1nX0 11B0 1nX0 11B0 1nX0 14p0 1lb0 14p0 1lb0 14p0 1lb0 14p0 1nX0 11B0 1nX0 11B0 1nX0 14p0 1lb0 14p0 1lb0 14p0 1nX0 11B0 1nX0 11B0 1nX0 Rd0 1zb0 Op0 1zb0 Op0 1zb0 Rd0 1zb0 Op0 1zb0 Op0 1zb0 Op0 1zb0 Op0 1zb0 Op0 1zb0 Rd0 1zb0 Op0 1zb0 Op0 1zb0 Op0 1zb0 Op0 1zb0 Rd0 1zb0 Op0 1zb0 Op0 1zb0 Op0 1zb0 Op0 1zb0 Op0 1zb0 Rd0 1zb0 Op0 1zb0 Op0 1zb0 Op0 1zb0 Op0 1zb0 Rd0 1zb0 Op0 1zb0 Op0 1zb0 Op0 1zb0 Op0 1zb0 Op0 1zb0|10e5');
var _elm_community$elm_time$Time_TimeZoneData$america_eirunepe_l = _elm_community$elm_time$Time_TimeZoneData$unpack('America/Eirunepe|LMT ACT ACST AMT|4D.s 50 40 40|0121212121212121212121212121212131|-2glvk.w HdLk.w 1cc0 1e10 1bX0 Ezd0 So0 1vA0 Mn0 1BB0 ML0 1BB0 zX0 qe10 xb0 2ep0 nz0 1C10 zX0 1C10 LX0 1C10 Mn0 H210 Rb0 1tB0 IL0 1Fd0 FX0 dPB0 On0 yTd0 d5X0|31e3');
var _elm_community$elm_time$Time_TimeZoneData$america_el_salvador_l = _elm_community$elm_time$Time_TimeZoneData$unpack('America/El_Salvador|LMT CST CDT|5U.M 60 50|012121|-1XiG3.c 2Fvc3.c WL0 1qN0 WL0|11e5');
var _elm_community$elm_time$Time_TimeZoneData$america_fort_nelson_l = _elm_community$elm_time$Time_TimeZoneData$unpack('America/Fort_Nelson|PST PDT PWT PPT MST|80 70 70 70 70|01023010101010101010101010101010101010101010101010101010101010101010101010101010101010101010101010101010101010101010101010101010101010101010104|-25TO0 1in0 UGp0 8x10 iy0 3NB0 11z0 1o10 11z0 1o10 11z0 1qN0 WL0 1qN0 11z0 1o10 11z0 1o10 11z0 1o10 11z0 1o10 11z0 1qN0 11z0 1o10 11z0 1o10 11z0 1o10 11z0 1o10 11z0 1qN0 WL0 1qN0 1cL0 1cN0 1cL0 1cN0 1cL0 1cN0 1fz0 1a10 1fz0 1cN0 1cL0 1cN0 1cL0 1cN0 1cL0 1cN0 1cL0 1cN0 1fz0 1cN0 1cL0 1cN0 1cL0 1cN0 1cL0 1cN0 1cL0 1cN0 1fz0 1a10 1fz0 1cN0 1cL0 1cN0 1cL0 1cN0 1cL0 1cN0 1cL0 1cN0 1fz0 1a10 1fz0 1cN0 1cL0 1cN0 1cL0 1cN0 1cL0 14p0 1lb0 14p0 1nX0 11B0 1nX0 11B0 1nX0 14p0 1lb0 14p0 1lb0 14p0 1nX0 11B0 1nX0 11B0 1nX0 14p0 1lb0 14p0 1lb0 14p0 1lb0 14p0 1nX0 11B0 1nX0 11B0 1nX0 14p0 1lb0 14p0 1lb0 14p0 1nX0 11B0 1nX0 11B0 1nX0 Rd0 1zb0 Op0 1zb0 Op0 1zb0 Rd0 1zb0 Op0 1zb0 Op0 1zb0 Op0 1zb0 Op0 1zb0 Op0|39e2');
var _elm_community$elm_time$Time_TimeZoneData$america_fort_wayne_l = _elm_community$elm_time$Time_TimeZoneData$unpack('America/Fort_Wayne|CST CDT CWT CPT EST EDT|60 50 50 50 50 40|010101023010101010101010101040454545454545454545454545454545454545454545454545454545454545454545454|-261s0 1nX0 11B0 1nX0 QI10 Db0 RB0 8x30 iw0 1o10 11z0 1o10 11z0 1o10 11z0 1o10 11z0 1qN0 WL0 1qN0 11z0 1o10 11z0 1o10 11z0 1o10 11z0 1o10 5Tz0 1o10 qLb0 1cL0 1cN0 1cL0 1qhd0 1nX0 Rd0 1zb0 Op0 1zb0 Op0 1zb0 Rd0 1zb0 Op0 1zb0 Op0 1zb0 Op0 1zb0 Op0 1zb0 Op0 1zb0 Rd0 1zb0 Op0 1zb0 Op0 1zb0 Op0 1zb0 Op0 1zb0 Rd0 1zb0 Op0 1zb0 Op0 1zb0 Op0 1zb0 Op0 1zb0 Op0 1zb0 Rd0 1zb0 Op0 1zb0 Op0 1zb0 Op0 1zb0 Op0 1zb0 Rd0 1zb0 Op0 1zb0 Op0 1zb0 Op0 1zb0 Op0 1zb0 Op0 1zb0');
var _elm_community$elm_time$Time_TimeZoneData$america_fortaleza_l = _elm_community$elm_time$Time_TimeZoneData$unpack('America/Fortaleza|LMT BRT BRST|2y 30 20|0121212121212121212121212121212121212121|-2glxq HdLq 1cc0 1e10 1bX0 Ezd0 So0 1vA0 Mn0 1BB0 ML0 1BB0 zX0 qe10 xb0 2ep0 nz0 1C10 zX0 1C10 LX0 1C10 Mn0 H210 Rb0 1tB0 IL0 1Fd0 FX0 1EN0 FX0 1HB0 Lz0 nsp0 WL0 1tB0 5z0 2mN0 On0|34e5');
var _elm_community$elm_time$Time_TimeZoneData$america_glace_bay_l = _elm_community$elm_time$Time_TimeZoneData$unpack('America/Glace_Bay|LMT AST ADT AWT APT|3X.M 40 30 30 30|012134121212121212121212121212121212121212121212121212121212121212121212121212121212121212121212121212121212121212121212121212121212121212121|-2IsI0.c CwO0.c 1in0 UGp0 8x50 iu0 iq10 11z0 Jg10 1cL0 1cN0 1cL0 1cN0 1cL0 1cN0 1cL0 1cN0 1fz0 1a10 1fz0 1cN0 1cL0 1cN0 1cL0 1cN0 1cL0 1cN0 1cL0 1cN0 1fz0 1a10 1fz0 1cN0 1cL0 1cN0 1cL0 1cN0 1cL0 14p0 1lb0 14p0 1nX0 11B0 1nX0 11B0 1nX0 14p0 1lb0 14p0 1lb0 14p0 1nX0 11B0 1nX0 11B0 1nX0 14p0 1lb0 14p0 1lb0 14p0 1lb0 14p0 1nX0 11B0 1nX0 11B0 1nX0 14p0 1lb0 14p0 1lb0 14p0 1nX0 11B0 1nX0 11B0 1nX0 Rd0 1zb0 Op0 1zb0 Op0 1zb0 Rd0 1zb0 Op0 1zb0 Op0 1zb0 Op0 1zb0 Op0 1zb0 Op0 1zb0 Rd0 1zb0 Op0 1zb0 Op0 1zb0 Op0 1zb0 Op0 1zb0 Rd0 1zb0 Op0 1zb0 Op0 1zb0 Op0 1zb0 Op0 1zb0 Op0 1zb0 Rd0 1zb0 Op0 1zb0 Op0 1zb0 Op0 1zb0 Op0 1zb0 Rd0 1zb0 Op0 1zb0 Op0 1zb0 Op0 1zb0 Op0 1zb0 Op0 1zb0|19e3');
var _elm_community$elm_time$Time_TimeZoneData$america_godthab_l = _elm_community$elm_time$Time_TimeZoneData$unpack('America/Godthab|LMT WGT WGST|3q.U 30 20|0121212121212121212121212121212121212121212121212121212121212121212121212121212121212121212121212121212121212121212121|-2a5Ux.4 2z5dx.4 19U0 1cM0 1cM0 1cM0 1cM0 1cM0 1cM0 1cM0 1fA0 1cM0 1cM0 1cM0 1cM0 1cM0 1cM0 1cM0 1cM0 1cM0 1cM0 1cM0 1fA0 1cM0 1cM0 1cM0 1cM0 1cM0 1cM0 1cM0 1cM0 1cM0 1cM0 1fA0 1o00 11A0 1o00 11A0 1o00 11A0 1qM0 WM0 1qM0 WM0 1qM0 11A0 1o00 11A0 1o00 11A0 1qM0 WM0 1qM0 WM0 1qM0 WM0 1qM0 11A0 1o00 11A0 1o00 11A0 1qM0 WM0 1qM0 WM0 1qM0 11A0 1o00 11A0 1o00 11A0 1o00 11A0 1qM0 WM0 1qM0 WM0 1qM0 11A0 1o00 11A0 1o00 11A0 1qM0 WM0 1qM0 WM0 1qM0 11A0 1o00 11A0 1o00 11A0 1o00 11A0 1qM0 WM0 1qM0 WM0 1qM0 11A0 1o00 11A0 1o00 11A0 1qM0 WM0 1qM0 WM0 1qM0 WM0 1qM0 11A0 1o00 11A0 1o00|17e3');
var _elm_community$elm_time$Time_TimeZoneData$america_goose_bay_l = _elm_community$elm_time$Time_TimeZoneData$unpack('America/Goose_Bay|NST NDT NST NDT NWT NPT AST ADT ADDT|3u.Q 2u.Q 3u 2u 2u 2u 40 30 20|010232323232323245232323232323232323232323232323232323232326767676767676767676767676767676767676767676768676767676767676767676767676767676767676767676767676767676767676767676767676767676767676767676767676|-25TSt.8 1in0 DXb0 2HbX.8 WL0 1qN0 WL0 1qN0 WL0 1tB0 TX0 1tB0 WL0 1qN0 WL0 1qN0 7UHu itu 1tB0 WL0 1qN0 WL0 1qN0 WL0 1qN0 WL0 1tB0 WL0 1ld0 11z0 1o10 11z0 1o10 11z0 1o10 11z0 1o10 11z0 1qN0 11z0 1o10 11z0 1o10 11z0 1o10 11z0 1o10 1fz0 1cN0 1cL0 1cN0 1cL0 1cN0 1cL0 1cN0 1cL0 1cN0 1fz0 S10 g0u 1fz0 1cN0 1cL0 1cN0 1cL0 1cN0 1cL0 1cN0 1cL0 1cN0 1fz0 1cN0 1cL0 1cN0 1cL0 1cN0 1cL0 1cN0 1cL0 1cN0 1fz0 1a10 1fz0 1cN0 1cL0 1cN0 1cL0 1cN0 1cL0 1cN0 1cL0 1cN0 1fz0 1a10 1fz0 1cN0 1cL0 1cN0 1cL0 1cN0 1cL0 14n1 1lb0 14p0 1nW0 11C0 1nX0 11B0 1nX0 14p0 1lb0 14p0 1lb0 14p0 1nX0 11B0 1nX0 11B0 1nX0 14p0 1lb0 14p0 1lb0 14p0 1lb0 14p0 1nX0 11B0 1nX0 11B0 1nX0 14p0 1lb0 14p0 1lb0 14p0 1nX0 11B0 1nX0 11B0 1nX0 Rd0 1zb0 Op0 1zb0 Op0 1zb0 Rd0 1zb0 Op0 1zcX Op0 1zb0 Op0 1zb0 Op0 1zb0 Op0 1zb0 Rd0 1zb0 Op0 1zb0 Op0 1zb0 Op0 1zb0 Op0 1zb0 Rd0 1zb0 Op0 1zb0 Op0 1zb0 Op0 1zb0 Op0 1zb0 Op0 1zb0 Rd0 1zb0 Op0 1zb0 Op0 1zb0 Op0 1zb0 Op0 1zb0 Rd0 1zb0 Op0 1zb0 Op0 1zb0 Op0 1zb0 Op0 1zb0 Op0 1zb0|76e2');
var _elm_community$elm_time$Time_TimeZoneData$america_grand_turk_l = _elm_community$elm_time$Time_TimeZoneData$unpack('America/Grand_Turk|KMT EST EDT AST|57.b 50 40 40|0121212121212121212121212121212121212121212121212121212121212121212121212123|-2l1uQ.N 2HHBQ.N 1cL0 1cN0 1cL0 1cN0 1cL0 1cN0 1fz0 1a10 1fz0 1cN0 1cL0 1cN0 1cL0 1cN0 1cL0 14p0 1lb0 14p0 1nX0 11B0 1nX0 11B0 1nX0 14p0 1lb0 14p0 1lb0 14p0 1nX0 11B0 1nX0 11B0 1nX0 14p0 1lb0 14p0 1lb0 14p0 1lb0 14p0 1nX0 11B0 1nX0 11B0 1nX0 14p0 1lb0 14p0 1lb0 14p0 1nX0 11B0 1nX0 11B0 1nX0 Rd0 1zb0 Op0 1zb0 Op0 1zb0 Rd0 1zb0 Op0 1zb0 Op0 1zb0 Op0 1zb0 Op0 1zb0 Op0 1zb0|37e2');
var _elm_community$elm_time$Time_TimeZoneData$america_guatemala_l = _elm_community$elm_time$Time_TimeZoneData$unpack('America/Guatemala|LMT CST CDT|62.4 60 50|0121212121|-24KhV.U 2efXV.U An0 mtd0 Nz0 ifB0 17b0 zDB0 11z0|13e5');
var _elm_community$elm_time$Time_TimeZoneData$america_guayaquil_l = _elm_community$elm_time$Time_TimeZoneData$unpack('America/Guayaquil|QMT ECT|5e 50|01|-1yVSK|27e5');
var _elm_community$elm_time$Time_TimeZoneData$america_guyana_l = _elm_community$elm_time$Time_TimeZoneData$unpack('America/Guyana|LMT GBGT GYT GYT GYT|3Q.E 3J 3J 30 40|01234|-2dvU7.k 24JzQ.k mlc0 Bxbf|80e4');
var _elm_community$elm_time$Time_TimeZoneData$america_halifax_l = _elm_community$elm_time$Time_TimeZoneData$unpack('America/Halifax|LMT AST ADT AWT APT|4e.o 40 30 30 30|0121212121212121212121212121212121212121212121212134121212121212121212121212121212121212121212121212121212121212121212121212121212121212121212121212121212121212121212121212121212121212121212121212121212121212121212121212121212121|-2IsHJ.A xzzJ.A 1db0 3I30 1in0 3HX0 IL0 1E10 ML0 1yN0 Pb0 1Bd0 Mn0 1Bd0 Rz0 1w10 Xb0 1w10 LX0 1w10 Xb0 1w10 Lz0 1C10 Jz0 1E10 OL0 1yN0 Un0 1qp0 Xb0 1qp0 11X0 1w10 Lz0 1HB0 LX0 1C10 FX0 1w10 Xb0 1qp0 Xb0 1BB0 LX0 1td0 Xb0 1qp0 Xb0 Rf0 8x50 iu0 1o10 11z0 1o10 11z0 1o10 11z0 1o10 11z0 3Qp0 11z0 1o10 11z0 1o10 11z0 1o10 11z0 3Qp0 11z0 1o10 11z0 1o10 11z0 1o10 11z0 6i10 1cL0 1cN0 1cL0 1cN0 1cL0 1cN0 1fz0 1a10 1fz0 1cN0 1cL0 1cN0 1cL0 1cN0 1cL0 1cN0 1cL0 1cN0 1fz0 1cN0 1cL0 1cN0 1cL0 1cN0 1cL0 1cN0 1cL0 1cN0 1fz0 1a10 1fz0 1cN0 1cL0 1cN0 1cL0 1cN0 1cL0 1cN0 1cL0 1cN0 1fz0 1a10 1fz0 1cN0 1cL0 1cN0 1cL0 1cN0 1cL0 14p0 1lb0 14p0 1nX0 11B0 1nX0 11B0 1nX0 14p0 1lb0 14p0 1lb0 14p0 1nX0 11B0 1nX0 11B0 1nX0 14p0 1lb0 14p0 1lb0 14p0 1lb0 14p0 1nX0 11B0 1nX0 11B0 1nX0 14p0 1lb0 14p0 1lb0 14p0 1nX0 11B0 1nX0 11B0 1nX0 Rd0 1zb0 Op0 1zb0 Op0 1zb0 Rd0 1zb0 Op0 1zb0 Op0 1zb0 Op0 1zb0 Op0 1zb0 Op0 1zb0 Rd0 1zb0 Op0 1zb0 Op0 1zb0 Op0 1zb0 Op0 1zb0 Rd0 1zb0 Op0 1zb0 Op0 1zb0 Op0 1zb0 Op0 1zb0 Op0 1zb0 Rd0 1zb0 Op0 1zb0 Op0 1zb0 Op0 1zb0 Op0 1zb0 Rd0 1zb0 Op0 1zb0 Op0 1zb0 Op0 1zb0 Op0 1zb0 Op0 1zb0|39e4');
var _elm_community$elm_time$Time_TimeZoneData$america_havana_l = _elm_community$elm_time$Time_TimeZoneData$unpack('America/Havana|HMT CST CDT|5t.A 50 40|012121212121212121212121212121212121212121212121212121212121212121212121212121212121212121212121212121212121212121212121212121212121212121212121212121212121|-1Meuu.o 72zu.o ML0 sld0 An0 1Nd0 Db0 1Nd0 An0 6Ep0 An0 1Nd0 An0 JDd0 Mn0 1Ap0 On0 1fd0 11X0 1qN0 WL0 1wp0 1cL0 1cN0 1cL0 1cN0 1fz0 1cN0 14n0 1ld0 14L0 1kN0 15b0 1kp0 1cL0 1cN0 1fz0 1a10 1fz0 1fB0 11z0 14p0 1nX0 11B0 1nX0 1o10 11z0 1o10 11z0 1o10 11z0 1o10 14n0 1ld0 14n0 11B0 1nX0 11B0 1nX0 14p0 1lb0 14p0 1lb0 1a10 1in0 1a10 1fA0 1a00 1fA0 1a00 1fA0 1a00 1fA0 1a00 1fA0 1cM0 1cM0 1cM0 1fA0 17c0 1o00 11A0 1qM0 11A0 1o00 11A0 1o00 14o0 1lc0 14o0 1lc0 11A0 6i00 Rc0 1wo0 U00 1tA0 Rc0 1wo0 U00 1wo0 U00 1zc0 U00 1qM0 Oo0 1zc0 Oo0 1zc0 Oo0 1zc0 Rc0 1zc0 Oo0 1zc0 Oo0 1zc0 Oo0 1zc0 Oo0 1zc0 Rc0 1zc0 Oo0 1zc0 Oo0 1zc0 Oo0 1zc0 Oo0 1zc0 Oo0 1zc0 Rc0 1zc0 Oo0 1zc0 Oo0 1zc0 Oo0 1zc0 Oo0 1zc0 Rc0 1zc0 Oo0 1zc0 Oo0 1zc0 Oo0 1zc0 Oo0 1zc0 Oo0 1zc0|21e5');
var _elm_community$elm_time$Time_TimeZoneData$america_hermosillo_l = _elm_community$elm_time$Time_TimeZoneData$unpack('America/Hermosillo|LMT MST CST PST MDT|7n.Q 70 60 80 60|0121212131414141|-1UQF0 deL0 8lc0 17c0 10M0 1dd0 otX0 gmN0 P2N0 13Vd0 1lb0 14p0 1lb0 14p0 1lb0|64e4');
var _elm_community$elm_time$Time_TimeZoneData$america_indiana_knox_l = _elm_community$elm_time$Time_TimeZoneData$unpack('America/Indiana/Knox|CST CDT CWT CPT EST|60 50 50 50 50|0101023010101010101010101010101010101040101010101010101010101010101010101010101010101010141010101010101010101010101010101010101010101010101010101010101010|-261s0 1nX0 11B0 1nX0 SgN0 8x30 iw0 3NB0 11z0 1o10 11z0 1o10 11z0 1qN0 WL0 1qN0 11z0 1o10 11z0 1o10 11z0 1o10 11z0 1o10 1fz0 1cN0 1cL0 1cN0 11z0 1o10 11z0 1o10 1cL0 1cN0 1fz0 1cN0 1cL0 1cN0 3Cn0 8wp0 1cL0 1cN0 1cL0 1cN0 1cL0 1cN0 1cL0 1cN0 1fz0 1cN0 1cL0 1cN0 1cL0 s10 1Vz0 LB0 1BX0 1cN0 1fz0 1a10 1fz0 1cN0 1cL0 1cN0 1cL0 1cN0 1cL0 1cN0 1cL0 1cN0 1fz0 1a10 1fz0 1cN0 1cL0 1cN0 1cL0 1cN0 1cL0 14p0 1lb0 14p0 1nX0 11B0 1nX0 11B0 1nX0 14p0 1lb0 z8o0 1o00 Rd0 1zb0 Op0 1zb0 Op0 1zb0 Rd0 1zb0 Op0 1zb0 Op0 1zb0 Op0 1zb0 Op0 1zb0 Op0 1zb0 Rd0 1zb0 Op0 1zb0 Op0 1zb0 Op0 1zb0 Op0 1zb0 Rd0 1zb0 Op0 1zb0 Op0 1zb0 Op0 1zb0 Op0 1zb0 Op0 1zb0 Rd0 1zb0 Op0 1zb0 Op0 1zb0 Op0 1zb0 Op0 1zb0 Rd0 1zb0 Op0 1zb0 Op0 1zb0 Op0 1zb0 Op0 1zb0 Op0 1zb0');
var _elm_community$elm_time$Time_TimeZoneData$america_indiana_marengo_l = _elm_community$elm_time$Time_TimeZoneData$unpack('America/Indiana/Marengo|CST CDT CWT CPT EST EDT|60 50 50 50 50 40|0101023010101010101010104545454545414545454545454545454545454545454545454545454545454545454545454545454|-261s0 1nX0 11B0 1nX0 SgN0 8x30 iw0 dyN0 11z0 6fd0 11z0 1o10 11z0 1qN0 11z0 1o10 11z0 1o10 11z0 1o10 11z0 1o10 11z0 1qN0 jrz0 1cL0 1cN0 1cL0 1cN0 1fz0 1cN0 1cL0 1cN0 1cL0 s10 1VA0 LA0 1BX0 1e6p0 1nX0 Rd0 1zb0 Op0 1zb0 Op0 1zb0 Rd0 1zb0 Op0 1zb0 Op0 1zb0 Op0 1zb0 Op0 1zb0 Op0 1zb0 Rd0 1zb0 Op0 1zb0 Op0 1zb0 Op0 1zb0 Op0 1zb0 Rd0 1zb0 Op0 1zb0 Op0 1zb0 Op0 1zb0 Op0 1zb0 Op0 1zb0 Rd0 1zb0 Op0 1zb0 Op0 1zb0 Op0 1zb0 Op0 1zb0 Rd0 1zb0 Op0 1zb0 Op0 1zb0 Op0 1zb0 Op0 1zb0 Op0 1zb0');
var _elm_community$elm_time$Time_TimeZoneData$america_indiana_petersburg_l = _elm_community$elm_time$Time_TimeZoneData$unpack('America/Indiana/Petersburg|CST CDT CWT CPT EST EDT|60 50 50 50 50 40|01010230101010101010101010104010101010101010101010141014545454545454545454545454545454545454545454545454545454545454|-261s0 1nX0 11B0 1nX0 SgN0 8x30 iw0 njX0 WN0 1qN0 11z0 1o10 11z0 1o10 11z0 1o10 11z0 1o10 11z0 1qN0 1cL0 1cN0 1cL0 1cN0 1cL0 1cN0 1cL0 1cN0 3Fb0 1cN0 1cL0 1cN0 1cL0 1cN0 1cL0 1cN0 1cL0 1cN0 1fz0 1cN0 1cL0 1cN0 1cL0 s10 1Vz0 LB0 1BX0 1cN0 1fz0 1a10 1fz0 19co0 1o00 Rd0 1zb0 Oo0 1zb0 Op0 1zb0 Rd0 1zb0 Op0 1zb0 Op0 1zb0 Op0 1zb0 Op0 1zb0 Op0 1zb0 Rd0 1zb0 Op0 1zb0 Op0 1zb0 Op0 1zb0 Op0 1zb0 Rd0 1zb0 Op0 1zb0 Op0 1zb0 Op0 1zb0 Op0 1zb0 Op0 1zb0 Rd0 1zb0 Op0 1zb0 Op0 1zb0 Op0 1zb0 Op0 1zb0 Rd0 1zb0 Op0 1zb0 Op0 1zb0 Op0 1zb0 Op0 1zb0 Op0 1zb0');
var _elm_community$elm_time$Time_TimeZoneData$america_indiana_tell_city_l = _elm_community$elm_time$Time_TimeZoneData$unpack('America/Indiana/Tell_City|CST CDT CWT CPT EST EDT|60 50 50 50 50 40|01010230101010101010101010101010454541010101010101010101010101010101010101010101010101010101010101010|-261s0 1nX0 11B0 1nX0 SgN0 8x30 iw0 1o10 11z0 g0p0 11z0 1o10 11z0 1qL0 WN0 1qN0 11z0 1o10 11z0 1o10 11z0 1o10 11z0 1o10 1fz0 1cN0 WL0 1qN0 1cL0 1cN0 1cL0 1cN0 caL0 1cL0 1cN0 1cL0 1qhd0 1o00 Rd0 1zb0 Op0 1zb0 Op0 1zb0 Rd0 1zb0 Op0 1zb0 Op0 1zb0 Op0 1zb0 Op0 1zb0 Op0 1zb0 Rd0 1zb0 Op0 1zb0 Op0 1zb0 Op0 1zb0 Op0 1zb0 Rd0 1zb0 Op0 1zb0 Op0 1zb0 Op0 1zb0 Op0 1zb0 Op0 1zb0 Rd0 1zb0 Op0 1zb0 Op0 1zb0 Op0 1zb0 Op0 1zb0 Rd0 1zb0 Op0 1zb0 Op0 1zb0 Op0 1zb0 Op0 1zb0 Op0 1zb0');
var _elm_community$elm_time$Time_TimeZoneData$america_indiana_vevay_l = _elm_community$elm_time$Time_TimeZoneData$unpack('America/Indiana/Vevay|CST CDT CWT CPT EST EDT|60 50 50 50 50 40|010102304545454545454545454545454545454545454545454545454545454545454545454545454|-261s0 1nX0 11B0 1nX0 SgN0 8x30 iw0 kPB0 Awn0 1cL0 1cN0 1cL0 1cN0 1fz0 1cN0 1cL0 1lnd0 1nX0 Rd0 1zb0 Op0 1zb0 Op0 1zb0 Rd0 1zb0 Op0 1zb0 Op0 1zb0 Op0 1zb0 Op0 1zb0 Op0 1zb0 Rd0 1zb0 Op0 1zb0 Op0 1zb0 Op0 1zb0 Op0 1zb0 Rd0 1zb0 Op0 1zb0 Op0 1zb0 Op0 1zb0 Op0 1zb0 Op0 1zb0 Rd0 1zb0 Op0 1zb0 Op0 1zb0 Op0 1zb0 Op0 1zb0 Rd0 1zb0 Op0 1zb0 Op0 1zb0 Op0 1zb0 Op0 1zb0 Op0 1zb0');
var _elm_community$elm_time$Time_TimeZoneData$america_indiana_vincennes_l = _elm_community$elm_time$Time_TimeZoneData$unpack('America/Indiana/Vincennes|CST CDT CWT CPT EST EDT|60 50 50 50 50 40|01010230101010101010101010101010454541014545454545454545454545454545454545454545454545454545454545454|-261s0 1nX0 11B0 1nX0 SgN0 8x30 iw0 1o10 11z0 g0p0 11z0 1o10 11z0 1qL0 WN0 1qN0 11z0 1o10 11z0 1o10 11z0 1o10 11z0 1o10 1fz0 1cN0 WL0 1qN0 1cL0 1cN0 1cL0 1cN0 caL0 1cL0 1cN0 1cL0 1qhd0 1o00 Rd0 1zb0 Oo0 1zb0 Op0 1zb0 Rd0 1zb0 Op0 1zb0 Op0 1zb0 Op0 1zb0 Op0 1zb0 Op0 1zb0 Rd0 1zb0 Op0 1zb0 Op0 1zb0 Op0 1zb0 Op0 1zb0 Rd0 1zb0 Op0 1zb0 Op0 1zb0 Op0 1zb0 Op0 1zb0 Op0 1zb0 Rd0 1zb0 Op0 1zb0 Op0 1zb0 Op0 1zb0 Op0 1zb0 Rd0 1zb0 Op0 1zb0 Op0 1zb0 Op0 1zb0 Op0 1zb0 Op0 1zb0');
var _elm_community$elm_time$Time_TimeZoneData$america_indiana_winamac_l = _elm_community$elm_time$Time_TimeZoneData$unpack('America/Indiana/Winamac|CST CDT CWT CPT EST EDT|60 50 50 50 50 40|01010230101010101010101010101010101010454541054545454545454545454545454545454545454545454545454545454545454|-261s0 1nX0 11B0 1nX0 SgN0 8x30 iw0 1o10 11z0 1o10 11z0 1o10 11z0 1o10 11z0 1qN0 WL0 1qN0 11z0 1o10 11z0 1o10 11z0 1o10 11z0 1o10 1fz0 1cN0 1cL0 1cN0 11z0 1o10 11z0 1o10 11z0 1o10 11z0 1qN0 jrz0 1cL0 1cN0 1cL0 1qhd0 1o00 Rd0 1za0 Op0 1zb0 Op0 1zb0 Rd0 1zb0 Op0 1zb0 Op0 1zb0 Op0 1zb0 Op0 1zb0 Op0 1zb0 Rd0 1zb0 Op0 1zb0 Op0 1zb0 Op0 1zb0 Op0 1zb0 Rd0 1zb0 Op0 1zb0 Op0 1zb0 Op0 1zb0 Op0 1zb0 Op0 1zb0 Rd0 1zb0 Op0 1zb0 Op0 1zb0 Op0 1zb0 Op0 1zb0 Rd0 1zb0 Op0 1zb0 Op0 1zb0 Op0 1zb0 Op0 1zb0 Op0 1zb0');
var _elm_community$elm_time$Time_TimeZoneData$america_inuvik_l = _elm_community$elm_time$Time_TimeZoneData$unpack('America/Inuvik|-00 PST PDDT MST MDT|0 80 60 70 60|0121343434343434343434343434343434343434343434343434343434343434343434343434343434343434343434343434343434343434343434343|-FnA0 tWU0 1fA0 wPe0 2pz0 1cL0 1cN0 1cL0 1cN0 1fz0 1a10 1fz0 1cN0 1cL0 1cN0 1cL0 1cN0 1cL0 14p0 1lb0 14p0 1nX0 11B0 1nX0 11B0 1nX0 14p0 1lb0 14p0 1lb0 14p0 1nX0 11B0 1nX0 11B0 1nX0 14p0 1lb0 14p0 1lb0 14p0 1lb0 14p0 1nX0 11B0 1nX0 11B0 1nX0 14p0 1lb0 14p0 1lb0 14p0 1nX0 11B0 1nX0 11B0 1nX0 Rd0 1zb0 Op0 1zb0 Op0 1zb0 Rd0 1zb0 Op0 1zb0 Op0 1zb0 Op0 1zb0 Op0 1zb0 Op0 1zb0 Rd0 1zb0 Op0 1zb0 Op0 1zb0 Op0 1zb0 Op0 1zb0 Rd0 1zb0 Op0 1zb0 Op0 1zb0 Op0 1zb0 Op0 1zb0 Op0 1zb0 Rd0 1zb0 Op0 1zb0 Op0 1zb0 Op0 1zb0 Op0 1zb0 Rd0 1zb0 Op0 1zb0 Op0 1zb0 Op0 1zb0 Op0 1zb0 Op0 1zb0|35e2');
var _elm_community$elm_time$Time_TimeZoneData$america_iqaluit_l = _elm_community$elm_time$Time_TimeZoneData$unpack('America/Iqaluit|-00 EWT EPT EST EDDT EDT CST CDT|0 40 40 50 30 40 60 50|01234353535353535353535353535353535353535353567353535353535353535353535353535353535353535353535353535353535353535353535353|-16K00 7nX0 iv0 LCL0 1fA0 zgO0 1cL0 1cN0 1cL0 1cN0 1fz0 1a10 1fz0 1cN0 1cL0 1cN0 1cL0 1cN0 1cL0 14p0 1lb0 14p0 1nX0 11B0 1nX0 11B0 1nX0 14p0 1lb0 14p0 1lb0 14p0 1nX0 11B0 1nX0 11B0 1nX0 14p0 1lb0 14p0 1lb0 14p0 1lb0 14p0 1nX0 11C0 1nX0 11A0 1nX0 14p0 1lb0 14p0 1lb0 14p0 1nX0 11B0 1nX0 11B0 1nX0 Rd0 1zb0 Op0 1zb0 Op0 1zb0 Rd0 1zb0 Op0 1zb0 Op0 1zb0 Op0 1zb0 Op0 1zb0 Op0 1zb0 Rd0 1zb0 Op0 1zb0 Op0 1zb0 Op0 1zb0 Op0 1zb0 Rd0 1zb0 Op0 1zb0 Op0 1zb0 Op0 1zb0 Op0 1zb0 Op0 1zb0 Rd0 1zb0 Op0 1zb0 Op0 1zb0 Op0 1zb0 Op0 1zb0 Rd0 1zb0 Op0 1zb0 Op0 1zb0 Op0 1zb0 Op0 1zb0 Op0 1zb0|67e2');
var _elm_community$elm_time$Time_TimeZoneData$america_jamaica_l = _elm_community$elm_time$Time_TimeZoneData$unpack('America/Jamaica|KMT EST EDT|57.b 50 40|0121212121212121212121|-2l1uQ.N 2uM1Q.N 1Vz0 LB0 1BX0 1cN0 1fz0 1a10 1fz0 1cN0 1cL0 1cN0 1cL0 1cN0 1cL0 1cN0 1cL0 1cN0 1fz0 1a10 1fz0|94e4');
var _elm_community$elm_time$Time_TimeZoneData$america_juneau_l = _elm_community$elm_time$Time_TimeZoneData$unpack('America/Juneau|PST PWT PPT PDT YDT YST AKST AKDT|80 70 70 70 80 90 90 80|01203030303030303030303030403030356767676767676767676767676767676767676767676767676767676767676767676767676767676767676767676767676767676767676|-17T20 8x10 iy0 Vo10 1cL0 1cN0 1cL0 1cN0 1fz0 1cN0 1cL0 1cN0 1cL0 s10 1Vz0 LB0 1BX0 1cN0 1fz0 1a10 1fz0 1cN0 1cL0 1cN0 1cL0 1cN0 1cM0 1cM0 1cL0 1cN0 1fz0 1a10 1fz0 co0 10q0 1cL0 1cN0 1cL0 1cN0 1cL0 14p0 1lb0 14p0 1nX0 11B0 1nX0 11B0 1nX0 14p0 1lb0 14p0 1lb0 14p0 1nX0 11B0 1nX0 11B0 1nX0 14p0 1lb0 14p0 1lb0 14p0 1lb0 14p0 1nX0 11B0 1nX0 11B0 1nX0 14p0 1lb0 14p0 1lb0 14p0 1nX0 11B0 1nX0 11B0 1nX0 Rd0 1zb0 Op0 1zb0 Op0 1zb0 Rd0 1zb0 Op0 1zb0 Op0 1zb0 Op0 1zb0 Op0 1zb0 Op0 1zb0 Rd0 1zb0 Op0 1zb0 Op0 1zb0 Op0 1zb0 Op0 1zb0 Rd0 1zb0 Op0 1zb0 Op0 1zb0 Op0 1zb0 Op0 1zb0 Op0 1zb0 Rd0 1zb0 Op0 1zb0 Op0 1zb0 Op0 1zb0 Op0 1zb0 Rd0 1zb0 Op0 1zb0 Op0 1zb0 Op0 1zb0 Op0 1zb0 Op0 1zb0|33e3');
var _elm_community$elm_time$Time_TimeZoneData$america_kentucky_louisville_l = _elm_community$elm_time$Time_TimeZoneData$unpack('America/Kentucky/Louisville|CST CDT CWT CPT EST EDT|60 50 50 50 50 40|0101010102301010101010101010101010101454545454545414545454545454545454545454545454545454545454545454545454545454545454545454545454545454545454545454545454545454545454545454545454|-261s0 1nX0 11B0 1nX0 3Fd0 Nb0 LPd0 11z0 RB0 8x30 iw0 Bb0 10N0 2bB0 8in0 1qN0 11z0 1o10 11z0 1o10 11z0 1o10 11z0 1o10 11z0 1qN0 1cL0 1cN0 1cL0 1cN0 1cL0 1cN0 1cL0 1cN0 1fz0 1cN0 xz0 gso0 1cL0 1cN0 1cL0 1cN0 1cL0 1cN0 1fz0 1cN0 1cL0 1cN0 1cL0 s10 1VA0 LA0 1BX0 1cN0 1fz0 1a10 1fz0 1cN0 1cL0 1cN0 1cL0 1cN0 1cL0 1cN0 1cL0 1cN0 1fz0 1a10 1fz0 1cN0 1cL0 1cN0 1cL0 1cN0 1cL0 14p0 1lb0 14p0 1nX0 11B0 1nX0 11B0 1nX0 14p0 1lb0 14p0 1lb0 14p0 1nX0 11B0 1nX0 11B0 1nX0 14p0 1lb0 14p0 1lb0 14p0 1lb0 14p0 1nX0 11B0 1nX0 11B0 1nX0 14p0 1lb0 14p0 1lb0 14p0 1nX0 11B0 1nX0 11B0 1nX0 Rd0 1zb0 Op0 1zb0 Op0 1zb0 Rd0 1zb0 Op0 1zb0 Op0 1zb0 Op0 1zb0 Op0 1zb0 Op0 1zb0 Rd0 1zb0 Op0 1zb0 Op0 1zb0 Op0 1zb0 Op0 1zb0 Rd0 1zb0 Op0 1zb0 Op0 1zb0 Op0 1zb0 Op0 1zb0 Op0 1zb0 Rd0 1zb0 Op0 1zb0 Op0 1zb0 Op0 1zb0 Op0 1zb0 Rd0 1zb0 Op0 1zb0 Op0 1zb0 Op0 1zb0 Op0 1zb0 Op0 1zb0');
var _elm_community$elm_time$Time_TimeZoneData$america_kentucky_monticello_l = _elm_community$elm_time$Time_TimeZoneData$unpack('America/Kentucky/Monticello|CST CDT CWT CPT EST EDT|60 50 50 50 50 40|0101023010101010101010101010101010101010101010101010101010101010101010101454545454545454545454545454545454545454545454545454545454545454545454545454|-261s0 1nX0 11B0 1nX0 SgN0 8x30 iw0 SWp0 1cL0 1cN0 1cL0 1cN0 1cL0 1cN0 1fz0 1cN0 1cL0 1cN0 1cL0 s10 1Vz0 LB0 1BX0 1cN0 1fz0 1a10 1fz0 1cN0 1cL0 1cN0 1cL0 1cN0 1cL0 1cN0 1cL0 1cN0 1fz0 1a10 1fz0 1cN0 1cL0 1cN0 1cL0 1cN0 1cL0 14p0 1lb0 14p0 1nX0 11B0 1nX0 11B0 1nX0 14p0 1lb0 14p0 1lb0 14p0 1nX0 11B0 1nX0 11B0 1nX0 14p0 1lb0 14p0 1lb0 14p0 1lb0 14p0 1nX0 11B0 1nX0 11A0 1nX0 14p0 1lb0 14p0 1lb0 14p0 1nX0 11B0 1nX0 11B0 1nX0 Rd0 1zb0 Op0 1zb0 Op0 1zb0 Rd0 1zb0 Op0 1zb0 Op0 1zb0 Op0 1zb0 Op0 1zb0 Op0 1zb0 Rd0 1zb0 Op0 1zb0 Op0 1zb0 Op0 1zb0 Op0 1zb0 Rd0 1zb0 Op0 1zb0 Op0 1zb0 Op0 1zb0 Op0 1zb0 Op0 1zb0 Rd0 1zb0 Op0 1zb0 Op0 1zb0 Op0 1zb0 Op0 1zb0 Rd0 1zb0 Op0 1zb0 Op0 1zb0 Op0 1zb0 Op0 1zb0 Op0 1zb0');
var _elm_community$elm_time$Time_TimeZoneData$america_la_paz_l = _elm_community$elm_time$Time_TimeZoneData$unpack('America/La_Paz|CMT BOST BOT|4w.A 3w.A 40|012|-1x37r.o 13b0|19e5');
var _elm_community$elm_time$Time_TimeZoneData$america_lima_l = _elm_community$elm_time$Time_TimeZoneData$unpack('America/Lima|LMT PET PEST|58.A 50 40|0121212121212121|-2tyGP.o 1bDzP.o zX0 1aN0 1cL0 1cN0 1cL0 1PrB0 zX0 1O10 zX0 6Gp0 zX0 98p0 zX0|11e6');
var _elm_community$elm_time$Time_TimeZoneData$america_los_angeles_l = _elm_community$elm_time$Time_TimeZoneData$unpack('America/Los_Angeles|PST PDT PWT PPT|80 70 70 70|010102301010101010101010101010101010101010101010101010101010101010101010101010101010101010101010101010101010101010101010101010101010101010101010101010101010101010101010101010101010101010|-261q0 1nX0 11B0 1nX0 SgN0 8x10 iy0 5Wp1 1VaX 3dA0 WM0 1qM0 11A0 1o00 11A0 1o00 11A0 1o00 11A0 1o00 11A0 1qM0 11A0 1o00 11A0 1o00 11A0 1o00 11A0 1o00 11A0 1qM0 WM0 1qM0 1cM0 1cM0 1cM0 1cM0 1cM0 1cM0 1fA0 1a00 1fA0 1cN0 1cL0 1cN0 1cL0 1cN0 1cL0 1cN0 1cL0 1cN0 1fz0 1cN0 1cL0 1cN0 1cL0 s10 1Vz0 LB0 1BX0 1cN0 1fz0 1a10 1fz0 1cN0 1cL0 1cN0 1cL0 1cN0 1cL0 1cN0 1cL0 1cN0 1fz0 1a10 1fz0 1cN0 1cL0 1cN0 1cL0 1cN0 1cL0 14p0 1lb0 14p0 1nX0 11B0 1nX0 11B0 1nX0 14p0 1lb0 14p0 1lb0 14p0 1nX0 11B0 1nX0 11B0 1nX0 14p0 1lb0 14p0 1lb0 14p0 1lb0 14p0 1nX0 11B0 1nX0 11B0 1nX0 14p0 1lb0 14p0 1lb0 14p0 1nX0 11B0 1nX0 11B0 1nX0 Rd0 1zb0 Op0 1zb0 Op0 1zb0 Rd0 1zb0 Op0 1zb0 Op0 1zb0 Op0 1zb0 Op0 1zb0 Op0 1zb0 Rd0 1zb0 Op0 1zb0 Op0 1zb0 Op0 1zb0 Op0 1zb0 Rd0 1zb0 Op0 1zb0 Op0 1zb0 Op0 1zb0 Op0 1zb0 Op0 1zb0 Rd0 1zb0 Op0 1zb0 Op0 1zb0 Op0 1zb0 Op0 1zb0 Rd0 1zb0 Op0 1zb0 Op0 1zb0 Op0 1zb0 Op0 1zb0 Op0 1zb0|15e6');
var _elm_community$elm_time$Time_TimeZoneData$america_maceio_l = _elm_community$elm_time$Time_TimeZoneData$unpack('America/Maceio|LMT BRT BRST|2m.Q 30 20|012121212121212121212121212121212121212121|-2glxB.8 HdLB.8 1cc0 1e10 1bX0 Ezd0 So0 1vA0 Mn0 1BB0 ML0 1BB0 zX0 qe10 xb0 2ep0 nz0 1C10 zX0 1C10 LX0 1C10 Mn0 H210 Rb0 1tB0 IL0 1Fd0 FX0 1EN0 FX0 1HB0 Lz0 dMN0 Lz0 8Q10 WL0 1tB0 5z0 2mN0 On0|93e4');
var _elm_community$elm_time$Time_TimeZoneData$america_managua_l = _elm_community$elm_time$Time_TimeZoneData$unpack('America/Managua|MMT CST EST CDT|5J.c 60 50 50|0121313121213131|-1quie.M 1yAMe.M 4mn0 9Up0 Dz0 1K10 Dz0 s3F0 1KH0 DB0 9In0 k8p0 19X0 1o30 11y0|22e5');
var _elm_community$elm_time$Time_TimeZoneData$america_manaus_l = _elm_community$elm_time$Time_TimeZoneData$unpack('America/Manaus|LMT AMT AMST|40.4 40 30|01212121212121212121212121212121|-2glvX.U HdKX.U 1cc0 1e10 1bX0 Ezd0 So0 1vA0 Mn0 1BB0 ML0 1BB0 zX0 qe10 xb0 2ep0 nz0 1C10 zX0 1C10 LX0 1C10 Mn0 H210 Rb0 1tB0 IL0 1Fd0 FX0 dPB0 On0|19e5');
var _elm_community$elm_time$Time_TimeZoneData$america_martinique_l = _elm_community$elm_time$Time_TimeZoneData$unpack('America/Martinique|FFMT AST ADT|44.k 40 30|0121|-2mPTT.E 2LPbT.E 19X0|39e4');
var _elm_community$elm_time$Time_TimeZoneData$america_matamoros_l = _elm_community$elm_time$Time_TimeZoneData$unpack('America/Matamoros|LMT CST CDT|6E 60 50|0121212121212121212121212121212121212121212121212121212121212121212121212121212121212121|-1UQG0 2FjC0 1nX0 i6p0 1lb0 14p0 1lb0 14p0 1lb0 14p0 1nX0 11B0 1nX0 1fB0 WL0 1fB0 1lb0 14p0 1lb0 14p0 1nX0 11B0 1nX0 11B0 1nX0 11B0 1nX0 14p0 1lb0 14p0 1lb0 U10 1zb0 Op0 1zb0 Op0 1zb0 Op0 1zb0 Op0 1zb0 Op0 1zb0 Rd0 1zb0 Op0 1zb0 Op0 1zb0 Op0 1zb0 Op0 1zb0 Rd0 1zb0 Op0 1zb0 Op0 1zb0 Op0 1zb0 Op0 1zb0 Op0 1zb0 Rd0 1zb0 Op0 1zb0 Op0 1zb0 Op0 1zb0 Op0 1zb0 Rd0 1zb0 Op0 1zb0 Op0 1zb0 Op0 1zb0 Op0 1zb0 Op0 1zb0|45e4');
var _elm_community$elm_time$Time_TimeZoneData$america_mazatlan_l = _elm_community$elm_time$Time_TimeZoneData$unpack('America/Mazatlan|LMT MST CST PST MDT|75.E 70 60 80 60|0121212131414141414141414141414141414141414141414141414141414141414141414141414141414141414141|-1UQF0 deL0 8lc0 17c0 10M0 1dd0 otX0 gmN0 P2N0 13Vd0 1lb0 14p0 1lb0 14p0 1lb0 14p0 1nX0 11B0 1nX0 1fB0 WL0 1fB0 1lb0 14p0 1lb0 14p0 1nX0 11B0 1nX0 11B0 1nX0 11B0 1nX0 14p0 1lb0 14p0 1lb0 14p0 1nX0 11B0 1nX0 11B0 1nX0 14p0 1lb0 14p0 1lb0 14p0 1lb0 14p0 1nX0 11B0 1nX0 11B0 1nX0 14p0 1lb0 14p0 1lb0 14p0 1nX0 11B0 1nX0 11B0 1nX0 14p0 1lb0 14p0 1lb0 14p0 1lb0 14p0 1nX0 11B0 1nX0 11B0 1nX0 14p0 1lb0 14p0 1lb0 14p0 1nX0 11B0 1nX0 11B0 1nX0 11B0 1nX0 14p0 1lb0 14p0 1lb0|44e4');
var _elm_community$elm_time$Time_TimeZoneData$america_menominee_l = _elm_community$elm_time$Time_TimeZoneData$unpack('America/Menominee|CST CDT CWT CPT EST|60 50 50 50 50|01010230101041010101010101010101010101010101010101010101010101010101010101010101010101010101010101010101010101010101010101010101010101010101010|-261s0 1nX0 11B0 1nX0 SgN0 8x30 iw0 1o10 11z0 LCN0 1fz0 6410 9Jb0 1cM0 s10 1Vz0 LB0 1BX0 1cN0 1fz0 1a10 1fz0 1cN0 1cL0 1cN0 1cL0 1cN0 1cL0 1cN0 1cL0 1cN0 1fz0 1a10 1fz0 1cN0 1cL0 1cN0 1cL0 1cN0 1cL0 14p0 1lb0 14p0 1nX0 11B0 1nX0 11B0 1nX0 14p0 1lb0 14p0 1lb0 14p0 1nX0 11B0 1nX0 11B0 1nX0 14p0 1lb0 14p0 1lb0 14p0 1lb0 14p0 1nX0 11B0 1nX0 11B0 1nX0 14p0 1lb0 14p0 1lb0 14p0 1nX0 11B0 1nX0 11B0 1nX0 Rd0 1zb0 Op0 1zb0 Op0 1zb0 Rd0 1zb0 Op0 1zb0 Op0 1zb0 Op0 1zb0 Op0 1zb0 Op0 1zb0 Rd0 1zb0 Op0 1zb0 Op0 1zb0 Op0 1zb0 Op0 1zb0 Rd0 1zb0 Op0 1zb0 Op0 1zb0 Op0 1zb0 Op0 1zb0 Op0 1zb0 Rd0 1zb0 Op0 1zb0 Op0 1zb0 Op0 1zb0 Op0 1zb0 Rd0 1zb0 Op0 1zb0 Op0 1zb0 Op0 1zb0 Op0 1zb0 Op0 1zb0|85e2');
var _elm_community$elm_time$Time_TimeZoneData$america_merida_l = _elm_community$elm_time$Time_TimeZoneData$unpack('America/Merida|LMT CST EST CDT|5W.s 60 50 50|0121313131313131313131313131313131313131313131313131313131313131313131313131313131313131|-1UQG0 2q2o0 2hz0 wu30 1lb0 14p0 1lb0 14p0 1lb0 14p0 1nX0 11B0 1nX0 1fB0 WL0 1fB0 1lb0 14p0 1lb0 14p0 1nX0 11B0 1nX0 11B0 1nX0 11B0 1nX0 14p0 1lb0 14p0 1lb0 14p0 1nX0 11B0 1nX0 11B0 1nX0 14p0 1lb0 14p0 1lb0 14p0 1lb0 14p0 1nX0 11B0 1nX0 11B0 1nX0 14p0 1lb0 14p0 1lb0 14p0 1nX0 11B0 1nX0 11B0 1nX0 14p0 1lb0 14p0 1lb0 14p0 1lb0 14p0 1nX0 11B0 1nX0 11B0 1nX0 14p0 1lb0 14p0 1lb0 14p0 1nX0 11B0 1nX0 11B0 1nX0 11B0 1nX0 14p0 1lb0 14p0 1lb0|11e5');
var _elm_community$elm_time$Time_TimeZoneData$america_metlakatla_l = _elm_community$elm_time$Time_TimeZoneData$unpack('America/Metlakatla|PST PWT PPT PDT AKST AKDT|80 70 70 70 90 80|0120303030303030303030303030303030454545454545454545454545454545454545454545454|-17T20 8x10 iy0 Vo10 1cL0 1cN0 1cL0 1cN0 1fz0 1cN0 1cL0 1cN0 1cL0 s10 1Vz0 LB0 1BX0 1cN0 1fz0 1a10 1fz0 1cN0 1cL0 1cN0 1cL0 1cN0 1cL0 1cN0 1cL0 1cN0 1fz0 1a10 1fz0 1hU10 Rd0 1zb0 Op0 1zb0 Op0 1zb0 Op0 1zb0 Op0 1zb0 Rd0 1zb0 Op0 1zb0 Op0 1zb0 Op0 1zb0 Op0 1zb0 Op0 1zb0 Rd0 1zb0 Op0 1zb0 Op0 1zb0 Op0 1zb0 Op0 1zb0 Rd0 1zb0 Op0 1zb0 Op0 1zb0 Op0 1zb0 Op0 1zb0 Op0 1zb0|14e2');
var _elm_community$elm_time$Time_TimeZoneData$america_mexico_city_l = _elm_community$elm_time$Time_TimeZoneData$unpack('America/Mexico_City|LMT MST CST CDT CWT|6A.A 70 60 50 50|012121232324232323232323232323232323232323232323232323232323232323232323232323232323232323232323232|-1UQF0 deL0 8lc0 17c0 10M0 1dd0 gEn0 TX0 3xd0 Jb0 6zB0 SL0 e5d0 17b0 1Pff0 1lb0 14p0 1lb0 14p0 1lb0 14p0 1nX0 11B0 1nX0 1fB0 WL0 1fB0 1lb0 14p0 1lb0 14p0 1nX0 11B0 1nX0 11B0 1nX0 11B0 1nX0 14p0 1lb0 14p0 1lb0 14p0 1nX0 11B0 1nX0 11B0 1nX0 14p0 1lb0 14p0 1lb0 14p0 1lb0 14p0 1nX0 11B0 1nX0 11B0 1nX0 14p0 1lb0 14p0 1lb0 14p0 1nX0 11B0 1nX0 11B0 1nX0 14p0 1lb0 14p0 1lb0 14p0 1lb0 14p0 1nX0 11B0 1nX0 11B0 1nX0 14p0 1lb0 14p0 1lb0 14p0 1nX0 11B0 1nX0 11B0 1nX0 11B0 1nX0 14p0 1lb0 14p0 1lb0|20e6');
var _elm_community$elm_time$Time_TimeZoneData$america_miquelon_l = _elm_community$elm_time$Time_TimeZoneData$unpack('America/Miquelon|LMT AST PMST PMDT|3I.E 40 30 20|012323232323232323232323232323232323232323232323232323232323232323232323232323232323232323232323232323232|-2mKkf.k 2LTAf.k gQ10 1lb0 14p0 1nX0 11B0 1nX0 11B0 1nX0 14p0 1lb0 14p0 1lb0 14p0 1nX0 11B0 1nX0 11B0 1nX0 14p0 1lb0 14p0 1lb0 14p0 1lb0 14p0 1nX0 11B0 1nX0 11B0 1nX0 14p0 1lb0 14p0 1lb0 14p0 1nX0 11B0 1nX0 11B0 1nX0 Rd0 1zb0 Op0 1zb0 Op0 1zb0 Rd0 1zb0 Op0 1zb0 Op0 1zb0 Op0 1zb0 Op0 1zb0 Op0 1zb0 Rd0 1zb0 Op0 1zb0 Op0 1zb0 Op0 1zb0 Op0 1zb0 Rd0 1zb0 Op0 1zb0 Op0 1zb0 Op0 1zb0 Op0 1zb0 Op0 1zb0 Rd0 1zb0 Op0 1zb0 Op0 1zb0 Op0 1zb0 Op0 1zb0 Rd0 1zb0 Op0 1zb0 Op0 1zb0 Op0 1zb0 Op0 1zb0 Op0 1zb0|61e2');
var _elm_community$elm_time$Time_TimeZoneData$america_moncton_l = _elm_community$elm_time$Time_TimeZoneData$unpack('America/Moncton|EST AST ADT AWT APT|50 40 30 30 30|012121212121212121212134121212121212121212121212121212121212121212121212121212121212121212121212121212121212121212121212121212121212121212121212121212121212121212121212121212121212121212121212121212121212121|-2IsH0 CwN0 1in0 zAo0 An0 1Nd0 An0 1Nd0 An0 1Nd0 An0 1Nd0 An0 1Nd0 An0 1K10 Lz0 1zB0 NX0 1u10 Wn0 S20 8x50 iu0 1o10 11z0 1o10 11z0 1o10 11z0 1o10 11z0 1qN0 WL0 1qN0 11z0 1o10 11z0 1o10 11z0 1o10 11z0 1o10 11z0 1qN0 11z0 1o10 1cL0 1cN0 1cL0 1cN0 1cL0 1cN0 1fz0 1cN0 1cL0 1cN0 1cL0 1cN0 1cL0 1cN0 1cL0 1cN0 1fz0 1a10 1fz0 1cN0 1cL0 1cN0 1cL0 1cN0 1cL0 1cN0 1cL0 1cN0 1fz0 1cN0 1cL0 3Cp0 1cL0 1cN0 1cL0 1cN0 1fz0 1a10 1fz0 1cN0 1cL0 1cN0 1cL0 1cN0 1cL0 1cN0 1cL0 1cN0 1fz0 1a10 1fz0 1cN0 1cL0 1cN0 1cL0 1cN0 1cL0 14p0 1lb0 14p0 1nX0 11B0 1nX0 11B0 1nX0 14p0 1lb0 14p0 1lb0 14n1 1nX0 11B0 1nX0 11B0 1nX0 14p0 1lb0 14p0 1lb0 14p0 1lb0 14p0 1nX0 11B0 1nX0 11B0 1nX0 14p0 1lb0 14p0 1lb0 14p0 1nX0 11B0 1nX0 11B0 1nX0 ReX 1zb0 Op0 1zb0 Op0 1zb0 Rd0 1zb0 Op0 1zb0 Op0 1zb0 Op0 1zb0 Op0 1zb0 Op0 1zb0 Rd0 1zb0 Op0 1zb0 Op0 1zb0 Op0 1zb0 Op0 1zb0 Rd0 1zb0 Op0 1zb0 Op0 1zb0 Op0 1zb0 Op0 1zb0 Op0 1zb0 Rd0 1zb0 Op0 1zb0 Op0 1zb0 Op0 1zb0 Op0 1zb0 Rd0 1zb0 Op0 1zb0 Op0 1zb0 Op0 1zb0 Op0 1zb0 Op0 1zb0|64e3');
var _elm_community$elm_time$Time_TimeZoneData$america_monterrey_l = _elm_community$elm_time$Time_TimeZoneData$unpack('America/Monterrey|LMT CST CDT|6F.g 60 50|0121212121212121212121212121212121212121212121212121212121212121212121212121212121212121|-1UQG0 2FjC0 1nX0 i6p0 1lb0 14p0 1lb0 14p0 1lb0 14p0 1nX0 11B0 1nX0 1fB0 WL0 1fB0 1lb0 14p0 1lb0 14p0 1nX0 11B0 1nX0 11B0 1nX0 11B0 1nX0 14p0 1lb0 14p0 1lb0 14p0 1nX0 11B0 1nX0 11B0 1nX0 14p0 1lb0 14p0 1lb0 14p0 1lb0 14p0 1nX0 11B0 1nX0 11B0 1nX0 14p0 1lb0 14p0 1lb0 14p0 1nX0 11B0 1nX0 11B0 1nX0 14p0 1lb0 14p0 1lb0 14p0 1lb0 14p0 1nX0 11B0 1nX0 11B0 1nX0 14p0 1lb0 14p0 1lb0 14p0 1nX0 11B0 1nX0 11B0 1nX0 11B0 1nX0 14p0 1lb0 14p0 1lb0|41e5');
var _elm_community$elm_time$Time_TimeZoneData$america_montevideo_l = _elm_community$elm_time$Time_TimeZoneData$unpack('America/Montevideo|MMT UYT UYHST UYST UYT UYHST|3I.I 3u 30 20 30 2u|012121212121212121212121213434343434345454543453434343434343434343434343434343434343434|-20UIf.g 8jzJ.g 1cLu 1dcu 1cLu 1dcu 1cLu ircu 11zu 1o0u 11zu 1o0u 11zu 1qMu WLu 1qMu WLu 1qMu WLu 1qMu 11zu 1o0u 11zu NAu 11bu 2iMu zWu Dq10 19X0 pd0 jz0 cm10 19X0 1fB0 1on0 11d0 1oL0 1nB0 1fzu 1aou 1fzu 1aou 1fzu 3nAu Jb0 3MN0 1SLu 4jzu 2PB0 Lb0 3Dd0 1pb0 ixd0 An0 1MN0 An0 1wp0 On0 1wp0 Rb0 1zd0 On0 1wp0 Rb0 s8p0 1fB0 1ip0 11z0 1ld0 14n0 1o10 11z0 1o10 11z0 1o10 14n0 1ld0 14n0 1ld0 14n0 1o10 11z0 1o10 11z0 1o10 11z0|17e5');
var _elm_community$elm_time$Time_TimeZoneData$america_nassau_l = _elm_community$elm_time$Time_TimeZoneData$unpack('America/Nassau|LMT EST EDT|59.u 50 40|012121212121212121212121212121212121212121212121212121212121212121212121212121212121212121212121212121212121212121212121212121212121212121212121212121|-2kNuO.u 26XdO.u 1cL0 1cN0 1fz0 1a10 1fz0 1cN0 1cL0 1cN0 1cL0 1cN0 1cL0 1cN0 1cL0 1cN0 1fz0 1cN0 1cL0 1cN0 1cL0 1cN0 1cL0 1cN0 1cL0 1cN0 1fz0 1a10 1fz0 1cN0 1cL0 1cN0 1cL0 1cN0 1cL0 1cN0 1cL0 1cN0 1fz0 1a10 1fz0 1cN0 1cL0 1cN0 1cL0 1cN0 1cL0 14p0 1lb0 14p0 1nX0 11B0 1nX0 11B0 1nX0 14p0 1lb0 14p0 1lb0 14p0 1nX0 11B0 1nX0 11B0 1nX0 14p0 1lb0 14p0 1lb0 14p0 1lb0 14p0 1nX0 11B0 1nX0 11B0 1nX0 14p0 1lb0 14p0 1lb0 14p0 1nX0 11B0 1nX0 11B0 1nX0 Rd0 1zb0 Op0 1zb0 Op0 1zb0 Rd0 1zb0 Op0 1zb0 Op0 1zb0 Op0 1zb0 Op0 1zb0 Op0 1zb0 Rd0 1zb0 Op0 1zb0 Op0 1zb0 Op0 1zb0 Op0 1zb0 Rd0 1zb0 Op0 1zb0 Op0 1zb0 Op0 1zb0 Op0 1zb0 Op0 1zb0 Rd0 1zb0 Op0 1zb0 Op0 1zb0 Op0 1zb0 Op0 1zb0 Rd0 1zb0 Op0 1zb0 Op0 1zb0 Op0 1zb0 Op0 1zb0 Op0 1zb0|24e4');
var _elm_community$elm_time$Time_TimeZoneData$america_new_york_l = _elm_community$elm_time$Time_TimeZoneData$unpack('America/New_York|EST EDT EWT EPT|50 40 40 40|01010101010101010101010101010101010101010101010102301010101010101010101010101010101010101010101010101010101010101010101010101010101010101010101010101010101010101010101010101010101010101010101010101010101010101010101010101010101010101010|-261t0 1nX0 11B0 1nX0 11B0 1qL0 1a10 11z0 1qN0 WL0 1qN0 11z0 1o10 11z0 1o10 11z0 1o10 11z0 1o10 11z0 1qN0 11z0 1o10 11z0 1o10 11z0 1o10 11z0 1o10 11z0 1qN0 WL0 1qN0 11z0 1o10 11z0 1o10 11z0 1o10 11z0 1o10 11z0 1qN0 WL0 1qN0 11z0 1o10 11z0 RB0 8x40 iv0 1o10 11z0 1o10 11z0 1o10 11z0 1o10 11z0 1qN0 WL0 1qN0 11z0 1o10 11z0 1o10 11z0 1o10 11z0 1o10 1fz0 1cN0 1cL0 1cN0 1cL0 1cN0 1cL0 1cN0 1cL0 1cN0 1fz0 1cN0 1cL0 1cN0 1cL0 1cN0 1cL0 1cN0 1cL0 1cN0 1fz0 1a10 1fz0 1cN0 1cL0 1cN0 1cL0 1cN0 1cL0 1cN0 1cL0 1cN0 1fz0 1cN0 1cL0 1cN0 1cL0 s10 1Vz0 LB0 1BX0 1cN0 1fz0 1a10 1fz0 1cN0 1cL0 1cN0 1cL0 1cN0 1cL0 1cN0 1cL0 1cN0 1fz0 1a10 1fz0 1cN0 1cL0 1cN0 1cL0 1cN0 1cL0 14p0 1lb0 14p0 1nX0 11B0 1nX0 11B0 1nX0 14p0 1lb0 14p0 1lb0 14p0 1nX0 11B0 1nX0 11B0 1nX0 14p0 1lb0 14p0 1lb0 14p0 1lb0 14p0 1nX0 11B0 1nX0 11B0 1nX0 14p0 1lb0 14p0 1lb0 14p0 1nX0 11B0 1nX0 11B0 1nX0 Rd0 1zb0 Op0 1zb0 Op0 1zb0 Rd0 1zb0 Op0 1zb0 Op0 1zb0 Op0 1zb0 Op0 1zb0 Op0 1zb0 Rd0 1zb0 Op0 1zb0 Op0 1zb0 Op0 1zb0 Op0 1zb0 Rd0 1zb0 Op0 1zb0 Op0 1zb0 Op0 1zb0 Op0 1zb0 Op0 1zb0 Rd0 1zb0 Op0 1zb0 Op0 1zb0 Op0 1zb0 Op0 1zb0 Rd0 1zb0 Op0 1zb0 Op0 1zb0 Op0 1zb0 Op0 1zb0 Op0 1zb0|21e6');
var _elm_community$elm_time$Time_TimeZoneData$america_nipigon_l = _elm_community$elm_time$Time_TimeZoneData$unpack('America/Nipigon|EST EDT EWT EPT|50 40 40 40|010123010101010101010101010101010101010101010101010101010101010101010101010101010101010101010101010101010101010101010101010101010101010|-25TR0 1in0 Rnb0 3je0 8x40 iv0 19yN0 1cL0 1cN0 1cL0 1cN0 1fz0 1a10 1fz0 1cN0 1cL0 1cN0 1cL0 1cN0 1cL0 1cN0 1cL0 1cN0 1fz0 1a10 1fz0 1cN0 1cL0 1cN0 1cL0 1cN0 1cL0 14p0 1lb0 14p0 1nX0 11B0 1nX0 11B0 1nX0 14p0 1lb0 14p0 1lb0 14p0 1nX0 11B0 1nX0 11B0 1nX0 14p0 1lb0 14p0 1lb0 14p0 1lb0 14p0 1nX0 11B0 1nX0 11B0 1nX0 14p0 1lb0 14p0 1lb0 14p0 1nX0 11B0 1nX0 11B0 1nX0 Rd0 1zb0 Op0 1zb0 Op0 1zb0 Rd0 1zb0 Op0 1zb0 Op0 1zb0 Op0 1zb0 Op0 1zb0 Op0 1zb0 Rd0 1zb0 Op0 1zb0 Op0 1zb0 Op0 1zb0 Op0 1zb0 Rd0 1zb0 Op0 1zb0 Op0 1zb0 Op0 1zb0 Op0 1zb0 Op0 1zb0 Rd0 1zb0 Op0 1zb0 Op0 1zb0 Op0 1zb0 Op0 1zb0 Rd0 1zb0 Op0 1zb0 Op0 1zb0 Op0 1zb0 Op0 1zb0 Op0 1zb0|16e2');
var _elm_community$elm_time$Time_TimeZoneData$america_nome_l = _elm_community$elm_time$Time_TimeZoneData$unpack('America/Nome|NST NWT NPT BST BDT YST AKST AKDT|b0 a0 a0 b0 a0 90 90 80|012034343434343434343434343434343456767676767676767676767676767676767676767676767676767676767676767676767676767676767676767676767676767676767676|-17SX0 8wW0 iB0 Qlb0 52O0 1cL0 1cN0 1cL0 1cN0 1fz0 1cN0 1cL0 1cN0 1cL0 s10 1Vz0 LB0 1BX0 1cN0 1fz0 1a10 1fz0 1cN0 1cL0 1cN0 1cL0 1cN0 1cL0 1cN0 1cL0 1cN0 1fz0 1a10 1fz0 cl0 10q0 1cL0 1cN0 1cL0 1cN0 1cL0 14p0 1lb0 14p0 1nX0 11B0 1nX0 11B0 1nX0 14p0 1lb0 14p0 1lb0 14p0 1nX0 11B0 1nX0 11B0 1nX0 14p0 1lb0 14p0 1lb0 14p0 1lb0 14p0 1nX0 11B0 1nX0 11B0 1nX0 14p0 1lb0 14p0 1lb0 14p0 1nX0 11B0 1nX0 11B0 1nX0 Rd0 1zb0 Op0 1zb0 Op0 1zb0 Rd0 1zb0 Op0 1zb0 Op0 1zb0 Op0 1zb0 Op0 1zb0 Op0 1zb0 Rd0 1zb0 Op0 1zb0 Op0 1zb0 Op0 1zb0 Op0 1zb0 Rd0 1zb0 Op0 1zb0 Op0 1zb0 Op0 1zb0 Op0 1zb0 Op0 1zb0 Rd0 1zb0 Op0 1zb0 Op0 1zb0 Op0 1zb0 Op0 1zb0 Rd0 1zb0 Op0 1zb0 Op0 1zb0 Op0 1zb0 Op0 1zb0 Op0 1zb0|38e2');
var _elm_community$elm_time$Time_TimeZoneData$america_noronha_l = _elm_community$elm_time$Time_TimeZoneData$unpack('America/Noronha|LMT FNT FNST|29.E 20 10|0121212121212121212121212121212121212121|-2glxO.k HdKO.k 1cc0 1e10 1bX0 Ezd0 So0 1vA0 Mn0 1BB0 ML0 1BB0 zX0 qe10 xb0 2ep0 nz0 1C10 zX0 1C10 LX0 1C10 Mn0 H210 Rb0 1tB0 IL0 1Fd0 FX0 1EN0 FX0 1HB0 Lz0 nsp0 WL0 1tB0 2L0 2pB0 On0|30e2');
var _elm_community$elm_time$Time_TimeZoneData$america_north_dakota_beulah_l = _elm_community$elm_time$Time_TimeZoneData$unpack('America/North_Dakota/Beulah|MST MDT MWT MPT CST CDT|70 60 60 60 60 50|010102301010101010101010101010101010101010101010101010101010101010101010101010101010101010101014545454545454545454545454545454545454545454545454545454|-261r0 1nX0 11B0 1nX0 SgN0 8x20 ix0 QwN0 1cL0 1cN0 1cL0 1cN0 1cL0 1cN0 1cL0 1cN0 1fz0 1cN0 1cL0 1cN0 1cL0 s10 1Vz0 LB0 1BX0 1cN0 1fz0 1a10 1fz0 1cN0 1cL0 1cN0 1cL0 1cN0 1cL0 1cN0 1cL0 1cN0 1fz0 1a10 1fz0 1cN0 1cL0 1cN0 1cL0 1cN0 1cL0 14p0 1lb0 14p0 1nX0 11B0 1nX0 11B0 1nX0 14p0 1lb0 14p0 1lb0 14p0 1nX0 11B0 1nX0 11B0 1nX0 14p0 1lb0 14p0 1lb0 14p0 1lb0 14p0 1nX0 11B0 1nX0 11B0 1nX0 14p0 1lb0 14p0 1lb0 14p0 1nX0 11B0 1nX0 11B0 1nX0 Rd0 1zb0 Op0 1zb0 Op0 1zb0 Rd0 1zb0 Oo0 1zb0 Op0 1zb0 Op0 1zb0 Op0 1zb0 Op0 1zb0 Rd0 1zb0 Op0 1zb0 Op0 1zb0 Op0 1zb0 Op0 1zb0 Rd0 1zb0 Op0 1zb0 Op0 1zb0 Op0 1zb0 Op0 1zb0 Op0 1zb0 Rd0 1zb0 Op0 1zb0 Op0 1zb0 Op0 1zb0 Op0 1zb0 Rd0 1zb0 Op0 1zb0 Op0 1zb0 Op0 1zb0 Op0 1zb0 Op0 1zb0');
var _elm_community$elm_time$Time_TimeZoneData$america_north_dakota_center_l = _elm_community$elm_time$Time_TimeZoneData$unpack('America/North_Dakota/Center|MST MDT MWT MPT CST CDT|70 60 60 60 60 50|010102301010101010101010101010101010101010101010101010101014545454545454545454545454545454545454545454545454545454545454545454545454545454545454545454|-261r0 1nX0 11B0 1nX0 SgN0 8x20 ix0 QwN0 1cL0 1cN0 1cL0 1cN0 1cL0 1cN0 1cL0 1cN0 1fz0 1cN0 1cL0 1cN0 1cL0 s10 1Vz0 LB0 1BX0 1cN0 1fz0 1a10 1fz0 1cN0 1cL0 1cN0 1cL0 1cN0 1cL0 1cN0 1cL0 1cN0 1fz0 1a10 1fz0 1cN0 1cL0 1cN0 1cL0 1cN0 1cL0 14p0 1lb0 14p0 1nX0 11B0 1nX0 11B0 1nX0 14p0 1lb0 14p0 1lb0 14o0 1nX0 11B0 1nX0 11B0 1nX0 14p0 1lb0 14p0 1lb0 14p0 1lb0 14p0 1nX0 11B0 1nX0 11B0 1nX0 14p0 1lb0 14p0 1lb0 14p0 1nX0 11B0 1nX0 11B0 1nX0 Rd0 1zb0 Op0 1zb0 Op0 1zb0 Rd0 1zb0 Op0 1zb0 Op0 1zb0 Op0 1zb0 Op0 1zb0 Op0 1zb0 Rd0 1zb0 Op0 1zb0 Op0 1zb0 Op0 1zb0 Op0 1zb0 Rd0 1zb0 Op0 1zb0 Op0 1zb0 Op0 1zb0 Op0 1zb0 Op0 1zb0 Rd0 1zb0 Op0 1zb0 Op0 1zb0 Op0 1zb0 Op0 1zb0 Rd0 1zb0 Op0 1zb0 Op0 1zb0 Op0 1zb0 Op0 1zb0 Op0 1zb0');
var _elm_community$elm_time$Time_TimeZoneData$america_north_dakota_new_salem_l = _elm_community$elm_time$Time_TimeZoneData$unpack('America/North_Dakota/New_Salem|MST MDT MWT MPT CST CDT|70 60 60 60 60 50|010102301010101010101010101010101010101010101010101010101010101010101010101010101454545454545454545454545454545454545454545454545454545454545454545454|-261r0 1nX0 11B0 1nX0 SgN0 8x20 ix0 QwN0 1cL0 1cN0 1cL0 1cN0 1cL0 1cN0 1cL0 1cN0 1fz0 1cN0 1cL0 1cN0 1cL0 s10 1Vz0 LB0 1BX0 1cN0 1fz0 1a10 1fz0 1cN0 1cL0 1cN0 1cL0 1cN0 1cL0 1cN0 1cL0 1cN0 1fz0 1a10 1fz0 1cN0 1cL0 1cN0 1cL0 1cN0 1cL0 14p0 1lb0 14p0 1nX0 11B0 1nX0 11B0 1nX0 14p0 1lb0 14p0 1lb0 14p0 1nX0 11B0 1nX0 11B0 1nX0 14p0 1lb0 14p0 1lb0 14p0 1lb0 14p0 1nX0 11B0 1nX0 11B0 1nX0 14p0 1lb0 14p0 1lb0 14o0 1nX0 11B0 1nX0 11B0 1nX0 Rd0 1zb0 Op0 1zb0 Op0 1zb0 Rd0 1zb0 Op0 1zb0 Op0 1zb0 Op0 1zb0 Op0 1zb0 Op0 1zb0 Rd0 1zb0 Op0 1zb0 Op0 1zb0 Op0 1zb0 Op0 1zb0 Rd0 1zb0 Op0 1zb0 Op0 1zb0 Op0 1zb0 Op0 1zb0 Op0 1zb0 Rd0 1zb0 Op0 1zb0 Op0 1zb0 Op0 1zb0 Op0 1zb0 Rd0 1zb0 Op0 1zb0 Op0 1zb0 Op0 1zb0 Op0 1zb0 Op0 1zb0');
var _elm_community$elm_time$Time_TimeZoneData$america_ojinaga_l = _elm_community$elm_time$Time_TimeZoneData$unpack('America/Ojinaga|LMT MST CST CDT MDT|6V.E 70 60 50 60|0121212323241414141414141414141414141414141414141414141414141414141414141414141414141414141|-1UQF0 deL0 8lc0 17c0 10M0 1dd0 2zQN0 1lb0 14p0 1lb0 14q0 1lb0 14p0 1nX0 11B0 1nX0 1fB0 WL0 1fB0 1lb0 14p0 1lb0 14p0 1nX0 11B0 1nX0 11B0 1nX0 11B0 1nX0 14p0 1lb0 14p0 1lb0 U10 1zb0 Op0 1zb0 Op0 1zb0 Op0 1zb0 Op0 1zb0 Op0 1zb0 Rd0 1zb0 Op0 1zb0 Op0 1zb0 Op0 1zb0 Op0 1zb0 Rd0 1zb0 Op0 1zb0 Op0 1zb0 Op0 1zb0 Op0 1zb0 Op0 1zb0 Rd0 1zb0 Op0 1zb0 Op0 1zb0 Op0 1zb0 Op0 1zb0 Rd0 1zb0 Op0 1zb0 Op0 1zb0 Op0 1zb0 Op0 1zb0 Op0 1zb0|23e3');
var _elm_community$elm_time$Time_TimeZoneData$america_panama_l = _elm_community$elm_time$Time_TimeZoneData$unpack('America/Panama|CMT EST|5j.A 50|01|-2uduE.o|15e5');
var _elm_community$elm_time$Time_TimeZoneData$america_pangnirtung_l = _elm_community$elm_time$Time_TimeZoneData$unpack('America/Pangnirtung|-00 AST AWT APT ADDT ADT EDT EST CST CDT|0 40 30 30 20 30 40 50 60 50|012314151515151515151515151515151515167676767689767676767676767676767676767676767676767676767676767676767676767676767676767|-1XiM0 PnG0 8x50 iu0 LCL0 1fA0 zgO0 1cL0 1cN0 1cL0 1cN0 1fz0 1a10 1fz0 1cN0 1cL0 1cN0 1cL0 1cN0 1cL0 14p0 1lb0 14p0 1nX0 11B0 1nX0 11B0 1nX0 14p0 1lb0 14p0 1lb0 14p0 1nX0 11B0 1nX0 11B0 1o00 14p0 1lb0 14p0 1lb0 14p0 1lb0 14p0 1nX0 11C0 1nX0 11A0 1nX0 14p0 1lb0 14p0 1lb0 14p0 1nX0 11B0 1nX0 11B0 1nX0 Rd0 1zb0 Op0 1zb0 Op0 1zb0 Rd0 1zb0 Op0 1zb0 Op0 1zb0 Op0 1zb0 Op0 1zb0 Op0 1zb0 Rd0 1zb0 Op0 1zb0 Op0 1zb0 Op0 1zb0 Op0 1zb0 Rd0 1zb0 Op0 1zb0 Op0 1zb0 Op0 1zb0 Op0 1zb0 Op0 1zb0 Rd0 1zb0 Op0 1zb0 Op0 1zb0 Op0 1zb0 Op0 1zb0 Rd0 1zb0 Op0 1zb0 Op0 1zb0 Op0 1zb0 Op0 1zb0 Op0 1zb0|14e2');
var _elm_community$elm_time$Time_TimeZoneData$america_paramaribo_l = _elm_community$elm_time$Time_TimeZoneData$unpack('America/Paramaribo|LMT PMT PMT NEGT SRT SRT|3E.E 3E.Q 3E.A 3u 3u 30|012345|-2nDUj.k Wqo0.c qanX.I 1dmLN.o lzc0|24e4');
var _elm_community$elm_time$Time_TimeZoneData$america_phoenix_l = _elm_community$elm_time$Time_TimeZoneData$unpack('America/Phoenix|MST MDT MWT|70 60 60|01010202010|-261r0 1nX0 11B0 1nX0 SgN0 4Al1 Ap0 1db0 SWqX 1cL0|42e5');
var _elm_community$elm_time$Time_TimeZoneData$america_port_au_prince_l = _elm_community$elm_time$Time_TimeZoneData$unpack('America/Port-au-Prince|PPMT EST EDT|4N 50 40|01212121212121212121212121212121212121212121|-28RHb 2FnMb 19X0 1cN0 1cL0 1cN0 1cL0 1cN0 1cL0 1cN0 1cL0 14q0 1o00 11A0 1o00 11A0 1o00 14o0 1lc0 14o0 1lc0 14o0 1o00 11A0 1o00 11A0 1o00 14o0 1lc0 14o0 1lc0 i6n0 1nX0 11B0 1nX0 d430 1zb0 Op0 1zb0 Op0 1zb0 Op0 1zb0|23e5');
var _elm_community$elm_time$Time_TimeZoneData$america_port_of_spain_l = _elm_community$elm_time$Time_TimeZoneData$unpack('America/Port_of_Spain|LMT AST|46.4 40|01|-2kNvR.U|43e3');
var _elm_community$elm_time$Time_TimeZoneData$america_porto_velho_l = _elm_community$elm_time$Time_TimeZoneData$unpack('America/Porto_Velho|LMT AMT AMST|4f.A 40 30|012121212121212121212121212121|-2glvI.o HdKI.o 1cc0 1e10 1bX0 Ezd0 So0 1vA0 Mn0 1BB0 ML0 1BB0 zX0 qe10 xb0 2ep0 nz0 1C10 zX0 1C10 LX0 1C10 Mn0 H210 Rb0 1tB0 IL0 1Fd0 FX0|37e4');
var _elm_community$elm_time$Time_TimeZoneData$america_puerto_rico_l = _elm_community$elm_time$Time_TimeZoneData$unpack('America/Puerto_Rico|AST AWT APT|40 30 30|0120|-17lU0 7XT0 iu0|24e5');
var _elm_community$elm_time$Time_TimeZoneData$america_rainy_river_l = _elm_community$elm_time$Time_TimeZoneData$unpack('America/Rainy_River|CST CDT CWT CPT|60 50 50 50|010123010101010101010101010101010101010101010101010101010101010101010101010101010101010101010101010101010101010101010101010101010101010|-25TQ0 1in0 Rnb0 3je0 8x30 iw0 19yN0 1cL0 1cN0 1cL0 1cN0 1fz0 1a10 1fz0 1cN0 1cL0 1cN0 1cL0 1cN0 1cL0 1cN0 1cL0 1cN0 1fz0 1a10 1fz0 1cN0 1cL0 1cN0 1cL0 1cN0 1cL0 14p0 1lb0 14p0 1nX0 11B0 1nX0 11B0 1nX0 14p0 1lb0 14p0 1lb0 14p0 1nX0 11B0 1nX0 11B0 1nX0 14p0 1lb0 14p0 1lb0 14p0 1lb0 14p0 1nX0 11B0 1nX0 11B0 1nX0 14p0 1lb0 14p0 1lb0 14p0 1nX0 11B0 1nX0 11B0 1nX0 Rd0 1zb0 Op0 1zb0 Op0 1zb0 Rd0 1zb0 Op0 1zb0 Op0 1zb0 Op0 1zb0 Op0 1zb0 Op0 1zb0 Rd0 1zb0 Op0 1zb0 Op0 1zb0 Op0 1zb0 Op0 1zb0 Rd0 1zb0 Op0 1zb0 Op0 1zb0 Op0 1zb0 Op0 1zb0 Op0 1zb0 Rd0 1zb0 Op0 1zb0 Op0 1zb0 Op0 1zb0 Op0 1zb0 Rd0 1zb0 Op0 1zb0 Op0 1zb0 Op0 1zb0 Op0 1zb0 Op0 1zb0|842');
var _elm_community$elm_time$Time_TimeZoneData$america_rankin_inlet_l = _elm_community$elm_time$Time_TimeZoneData$unpack('America/Rankin_Inlet|-00 CST CDDT CDT EST|0 60 40 50 50|012131313131313131313131313131313131313131313431313131313131313131313131313131313131313131313131313131313131313131313131|-vDc0 keu0 1fA0 zgO0 1cL0 1cN0 1cL0 1cN0 1fz0 1a10 1fz0 1cN0 1cL0 1cN0 1cL0 1cN0 1cL0 14p0 1lb0 14p0 1nX0 11B0 1nX0 11B0 1nX0 14p0 1lb0 14p0 1lb0 14p0 1nX0 11B0 1nX0 11B0 1nX0 14p0 1lb0 14p0 1lb0 14p0 1lb0 14p0 1nX0 11B0 1nX0 11B0 1nX0 14p0 1lb0 14p0 1lb0 14p0 1nX0 11B0 1nX0 11B0 1nX0 Rd0 1zb0 Op0 1zb0 Op0 1zb0 Rd0 1zb0 Op0 1zb0 Op0 1zb0 Op0 1zb0 Op0 1zb0 Op0 1zb0 Rd0 1zb0 Op0 1zb0 Op0 1zb0 Op0 1zb0 Op0 1zb0 Rd0 1zb0 Op0 1zb0 Op0 1zb0 Op0 1zb0 Op0 1zb0 Op0 1zb0 Rd0 1zb0 Op0 1zb0 Op0 1zb0 Op0 1zb0 Op0 1zb0 Rd0 1zb0 Op0 1zb0 Op0 1zb0 Op0 1zb0 Op0 1zb0 Op0 1zb0|26e2');
var _elm_community$elm_time$Time_TimeZoneData$america_recife_l = _elm_community$elm_time$Time_TimeZoneData$unpack('America/Recife|LMT BRT BRST|2j.A 30 20|0121212121212121212121212121212121212121|-2glxE.o HdLE.o 1cc0 1e10 1bX0 Ezd0 So0 1vA0 Mn0 1BB0 ML0 1BB0 zX0 qe10 xb0 2ep0 nz0 1C10 zX0 1C10 LX0 1C10 Mn0 H210 Rb0 1tB0 IL0 1Fd0 FX0 1EN0 FX0 1HB0 Lz0 nsp0 WL0 1tB0 2L0 2pB0 On0|33e5');
var _elm_community$elm_time$Time_TimeZoneData$america_regina_l = _elm_community$elm_time$Time_TimeZoneData$unpack('America/Regina|LMT MST MDT MWT MPT CST|6W.A 70 60 60 60 60|012121212121212121212121341212121212121212121212121215|-2AD51.o uHe1.o 1in0 s2L0 11z0 1o10 11z0 1o10 11z0 1qN0 WL0 1qN0 11z0 66N0 1cL0 1cN0 19X0 1fB0 1cL0 1fB0 1cL0 1cN0 1cL0 M30 8x20 ix0 1ip0 1cL0 1ip0 11z0 1o10 11z0 1o10 11z0 1qN0 WL0 1qN0 11z0 1o10 11z0 1o10 11z0 1o10 11z0 1o10 11z0 1qN0 11z0 1o10 11z0 3NB0 1cL0 1cN0|19e4');
var _elm_community$elm_time$Time_TimeZoneData$america_resolute_l = _elm_community$elm_time$Time_TimeZoneData$unpack('America/Resolute|-00 CST CDDT CDT EST|0 60 40 50 50|012131313131313131313131313131313131313131313431313131313431313131313131313131313131313131313131313131313131313131313131|-SnA0 GWS0 1fA0 zgO0 1cL0 1cN0 1cL0 1cN0 1fz0 1a10 1fz0 1cN0 1cL0 1cN0 1cL0 1cN0 1cL0 14p0 1lb0 14p0 1nX0 11B0 1nX0 11B0 1nX0 14p0 1lb0 14p0 1lb0 14p0 1nX0 11B0 1nX0 11B0 1nX0 14p0 1lb0 14p0 1lb0 14p0 1lb0 14p0 1nX0 11B0 1nX0 11B0 1nX0 14p0 1lb0 14p0 1lb0 14p0 1nX0 11B0 1nX0 11B0 1nX0 Rd0 1zb0 Op0 1zb0 Op0 1zb0 Rd0 1zb0 Op0 1zb0 Op0 1zb0 Op0 1zb0 Op0 1zb0 Op0 1zb0 Rd0 1zb0 Op0 1zb0 Op0 1zb0 Op0 1zb0 Op0 1zb0 Rd0 1zb0 Op0 1zb0 Op0 1zb0 Op0 1zb0 Op0 1zb0 Op0 1zb0 Rd0 1zb0 Op0 1zb0 Op0 1zb0 Op0 1zb0 Op0 1zb0 Rd0 1zb0 Op0 1zb0 Op0 1zb0 Op0 1zb0 Op0 1zb0 Op0 1zb0|229');
var _elm_community$elm_time$Time_TimeZoneData$america_rio_branco_l = _elm_community$elm_time$Time_TimeZoneData$unpack('America/Rio_Branco|LMT ACT ACST AMT|4v.c 50 40 40|01212121212121212121212121212131|-2glvs.M HdLs.M 1cc0 1e10 1bX0 Ezd0 So0 1vA0 Mn0 1BB0 ML0 1BB0 zX0 qe10 xb0 2ep0 nz0 1C10 zX0 1C10 LX0 1C10 Mn0 H210 Rb0 1tB0 IL0 1Fd0 FX0 NBd0 d5X0|31e4');
var _elm_community$elm_time$Time_TimeZoneData$america_santarem_l = _elm_community$elm_time$Time_TimeZoneData$unpack('America/Santarem|LMT AMT AMST BRT|3C.M 40 30 30|0121212121212121212121212121213|-2glwl.c HdLl.c 1cc0 1e10 1bX0 Ezd0 So0 1vA0 Mn0 1BB0 ML0 1BB0 zX0 qe10 xb0 2ep0 nz0 1C10 zX0 1C10 LX0 1C10 Mn0 H210 Rb0 1tB0 IL0 1Fd0 FX0 NBd0|21e4');
var _elm_community$elm_time$Time_TimeZoneData$america_santiago_l = _elm_community$elm_time$Time_TimeZoneData$unpack('America/Santiago|SMT CLT CLT CLST CLST|4G.K 50 40 40 30|010203131313131212421242424242424242424242424242424242424242424242424242424242424242424242424242424242424242424242424242424242424242424242424242424242424242424|-2q2jh.e fJAh.e 5knG.K 1Vzh.e jRAG.K 1pbh.e 11d0 1oL0 11d0 1oL0 11d0 1oL0 11d0 1pb0 11d0 nHX0 op0 9Bz0 jb0 1oN0 ko0 Qeo0 WL0 1zd0 On0 1ip0 11z0 1o10 11z0 1qN0 WL0 1ld0 14n0 1qN0 WL0 1qN0 11z0 1o10 11z0 1o10 11z0 1qN0 WL0 1qN0 WL0 1qN0 11z0 1o10 11z0 1o10 11z0 1o10 11z0 1qN0 WL0 1qN0 WL0 1qN0 1cL0 1cN0 11z0 1o10 11z0 1qN0 WL0 1fB0 19X0 1qN0 11z0 1o10 11z0 1o10 11z0 1o10 11z0 1qN0 WL0 1qN0 17b0 1ip0 11z0 1ip0 1fz0 1fB0 11z0 1qN0 WL0 1qN0 WL0 1qN0 WL0 1qN0 11z0 1o10 11z0 1o10 11z0 1qN0 WL0 1qN0 17b0 1ip0 11z0 1o10 19X0 1fB0 1nX0 G10 1EL0 Op0 1zb0 Rd0 1wn0 Rd0 46n0 Ap0 1Nb0 Ap0 1Nb0 Ap0 1Nb0 Ap0 1Nb0 Ap0 1Nb0 Dd0 1Nb0 Ap0 1Nb0 Ap0 1Nb0 Ap0 1Nb0 Ap0 1Nb0 Ap0 1Nb0 Dd0 1Nb0 Ap0 1Nb0 Ap0 1Nb0 Ap0 1Nb0 Ap0 1Nb0 Dd0 1Nb0 Ap0 1Nb0 Ap0 1Nb0 Ap0 1Nb0 Ap0 1Nb0 Ap0|62e5');
var _elm_community$elm_time$Time_TimeZoneData$america_santo_domingo_l = _elm_community$elm_time$Time_TimeZoneData$unpack('America/Santo_Domingo|SDMT EST EDT EHDT AST|4E 50 40 4u 40|01213131313131414|-1ttjk 1lJMk Mn0 6sp0 Lbu 1Cou yLu 1RAu wLu 1QMu xzu 1Q0u xXu 1PAu 13jB0 e00|29e5');
var _elm_community$elm_time$Time_TimeZoneData$america_sao_paulo_l = _elm_community$elm_time$Time_TimeZoneData$unpack('America/Sao_Paulo|LMT BRT BRST|36.s 30 20|012121212121212121212121212121212121212121212121212121212121212121212121212121212121212121212121212121212121212121212121212121212|-2glwR.w HdKR.w 1cc0 1e10 1bX0 Ezd0 So0 1vA0 Mn0 1BB0 ML0 1BB0 zX0 pTd0 PX0 2ep0 nz0 1C10 zX0 1C10 LX0 1C10 Mn0 H210 Rb0 1tB0 IL0 1Fd0 FX0 1EN0 FX0 1HB0 Lz0 1EN0 Lz0 1C10 IL0 1HB0 Db0 1HB0 On0 1zd0 On0 1zd0 Lz0 1zd0 Rb0 1wN0 Wn0 1tB0 Rb0 1tB0 WL0 1tB0 Rb0 1zd0 On0 1HB0 FX0 1C10 Lz0 1Ip0 HX0 1zd0 On0 1HB0 IL0 1wp0 On0 1C10 Lz0 1C10 On0 1zd0 On0 1zd0 Rb0 1zd0 Lz0 1C10 Lz0 1C10 On0 1zd0 On0 1zd0 On0 1zd0 On0 1C10 Lz0 1C10 Lz0 1C10 On0 1zd0 On0 1zd0 Rb0 1wp0 On0 1C10 Lz0 1C10 On0 1zd0 On0 1zd0 On0 1zd0 On0 1C10 Lz0 1C10 Lz0 1C10 Lz0 1C10 On0 1zd0 Rb0 1wp0 On0 1C10 Lz0 1C10 On0 1zd0|20e6');
var _elm_community$elm_time$Time_TimeZoneData$america_scoresbysund_l = _elm_community$elm_time$Time_TimeZoneData$unpack('America/Scoresbysund|LMT CGT CGST EGST EGT|1r.Q 20 10 0 10|0121343434343434343434343434343434343434343434343434343434343434343434343434343434343434343434343434343434343434343434|-2a5Ww.8 2z5ew.8 1a00 1cK0 1cL0 1cM0 1cM0 1cM0 1cM0 1cM0 1fA0 1cM0 1cM0 1cM0 1cM0 1cM0 1cM0 1cM0 1cM0 1cM0 1cM0 1cM0 1fA0 1cM0 1cM0 1cM0 1cM0 1cM0 1cM0 1cM0 1cM0 1cM0 1cM0 1fA0 1o00 11A0 1o00 11A0 1o00 11A0 1qM0 WM0 1qM0 WM0 1qM0 11A0 1o00 11A0 1o00 11A0 1qM0 WM0 1qM0 WM0 1qM0 WM0 1qM0 11A0 1o00 11A0 1o00 11A0 1qM0 WM0 1qM0 WM0 1qM0 11A0 1o00 11A0 1o00 11A0 1o00 11A0 1qM0 WM0 1qM0 WM0 1qM0 11A0 1o00 11A0 1o00 11A0 1qM0 WM0 1qM0 WM0 1qM0 11A0 1o00 11A0 1o00 11A0 1o00 11A0 1qM0 WM0 1qM0 WM0 1qM0 11A0 1o00 11A0 1o00 11A0 1qM0 WM0 1qM0 WM0 1qM0 WM0 1qM0 11A0 1o00 11A0 1o00|452');
var _elm_community$elm_time$Time_TimeZoneData$america_sitka_l = _elm_community$elm_time$Time_TimeZoneData$unpack('America/Sitka|PST PWT PPT PDT YST AKST AKDT|80 70 70 70 90 90 80|01203030303030303030303030303030345656565656565656565656565656565656565656565656565656565656565656565656565656565656565656565656565656565656565|-17T20 8x10 iy0 Vo10 1cL0 1cN0 1cL0 1cN0 1fz0 1cN0 1cL0 1cN0 1cL0 s10 1Vz0 LB0 1BX0 1cN0 1fz0 1a10 1fz0 1cN0 1cL0 1cN0 1cL0 1cN0 1cL0 1cN0 1cL0 1cN0 1fz0 1a10 1fz0 co0 10q0 1cL0 1cN0 1cL0 1cN0 1cL0 14p0 1lb0 14p0 1nX0 11B0 1nX0 11B0 1nX0 14p0 1lb0 14p0 1lb0 14p0 1nX0 11B0 1nX0 11B0 1nX0 14p0 1lb0 14p0 1lb0 14p0 1lb0 14p0 1nX0 11B0 1nX0 11B0 1nX0 14p0 1lb0 14p0 1lb0 14p0 1nX0 11B0 1nX0 11B0 1nX0 Rd0 1zb0 Op0 1zb0 Op0 1zb0 Rd0 1zb0 Op0 1zb0 Op0 1zb0 Op0 1zb0 Op0 1zb0 Op0 1zb0 Rd0 1zb0 Op0 1zb0 Op0 1zb0 Op0 1zb0 Op0 1zb0 Rd0 1zb0 Op0 1zb0 Op0 1zb0 Op0 1zb0 Op0 1zb0 Op0 1zb0 Rd0 1zb0 Op0 1zb0 Op0 1zb0 Op0 1zb0 Op0 1zb0 Rd0 1zb0 Op0 1zb0 Op0 1zb0 Op0 1zb0 Op0 1zb0 Op0 1zb0|90e2');
var _elm_community$elm_time$Time_TimeZoneData$america_st_johns_l = _elm_community$elm_time$Time_TimeZoneData$unpack('America/St_Johns|NST NDT NST NDT NWT NPT NDDT|3u.Q 2u.Q 3u 2u 2u 2u 1u|01010101010101010101010101010101010102323232323232324523232323232323232323232323232323232323232323232323232323232323232323232323232323232326232323232323232323232323232323232323232323232323232323232323232323232323232323232323232323232323232|-28oit.8 14L0 1nB0 1in0 1gm0 Dz0 1JB0 1cL0 1cN0 1cL0 1fB0 19X0 1fB0 19X0 1fB0 19X0 1fB0 19X0 1fB0 1cL0 1cN0 1cL0 1fB0 19X0 1fB0 19X0 1fB0 19X0 1fB0 19X0 1fB0 1cL0 1fB0 19X0 1fB0 19X0 10O0 eKX.8 19X0 1iq0 WL0 1qN0 WL0 1qN0 WL0 1tB0 TX0 1tB0 WL0 1qN0 WL0 1qN0 7UHu itu 1tB0 WL0 1qN0 WL0 1qN0 WL0 1qN0 WL0 1tB0 WL0 1ld0 11z0 1o10 11z0 1o10 11z0 1o10 11z0 1o10 11z0 1qN0 11z0 1o10 11z0 1o10 11z0 1o10 11z0 1o10 1fz0 1cN0 1cL0 1cN0 1cL0 1cN0 1cL0 1cN0 1cL0 1cN0 1fz0 1a10 1fz0 1cN0 1cL0 1cN0 1cL0 1cN0 1cL0 1cN0 1cL0 1cN0 1fz0 1cN0 1cL0 1cN0 1cL0 1cN0 1cL0 1cN0 1cL0 1cN0 1fz0 1a10 1fz0 1cN0 1cL0 1cN0 1cL0 1cN0 1cL0 1cN0 1cL0 1cN0 1fz0 1a10 1fz0 1cN0 1cL0 1cN0 1cL0 1cN0 1cL0 14n1 1lb0 14p0 1nW0 11C0 1nX0 11B0 1nX0 14p0 1lb0 14p0 1lb0 14p0 1nX0 11B0 1nX0 11B0 1nX0 14p0 1lb0 14p0 1lb0 14p0 1lb0 14p0 1nX0 11B0 1nX0 11B0 1nX0 14p0 1lb0 14p0 1lb0 14p0 1nX0 11B0 1nX0 11B0 1nX0 Rd0 1zb0 Op0 1zb0 Op0 1zb0 Rd0 1zb0 Op0 1zcX Op0 1zb0 Op0 1zb0 Op0 1zb0 Op0 1zb0 Rd0 1zb0 Op0 1zb0 Op0 1zb0 Op0 1zb0 Op0 1zb0 Rd0 1zb0 Op0 1zb0 Op0 1zb0 Op0 1zb0 Op0 1zb0 Op0 1zb0 Rd0 1zb0 Op0 1zb0 Op0 1zb0 Op0 1zb0 Op0 1zb0 Rd0 1zb0 Op0 1zb0 Op0 1zb0 Op0 1zb0 Op0 1zb0 Op0 1zb0|11e4');
var _elm_community$elm_time$Time_TimeZoneData$america_swift_current_l = _elm_community$elm_time$Time_TimeZoneData$unpack('America/Swift_Current|LMT MST MDT MWT MPT CST|7b.k 70 60 60 60 60|012134121212121212121215|-2AD4M.E uHdM.E 1in0 UGp0 8x20 ix0 1o10 17b0 1ip0 11z0 1o10 11z0 1o10 11z0 isN0 1cL0 3Cp0 1cL0 1cN0 11z0 1qN0 WL0 pMp0|16e3');
var _elm_community$elm_time$Time_TimeZoneData$america_tegucigalpa_l = _elm_community$elm_time$Time_TimeZoneData$unpack('America/Tegucigalpa|LMT CST CDT|5M.Q 60 50|01212121|-1WGGb.8 2ETcb.8 WL0 1qN0 WL0 GRd0 AL0|11e5');
var _elm_community$elm_time$Time_TimeZoneData$america_thule_l = _elm_community$elm_time$Time_TimeZoneData$unpack('America/Thule|LMT AST ADT|4z.8 40 30|012121212121212121212121212121212121212121212121212121212121212121212121212121212121212121212121|-2a5To.Q 31NBo.Q 1cL0 1cN0 1cL0 1fB0 1nX0 11B0 1nX0 11B0 1nX0 14p0 1lb0 14p0 1lb0 14p0 1lb0 14p0 1nX0 11B0 1nX0 11B0 1nX0 14p0 1lb0 14p0 1lb0 14p0 1nX0 11B0 1nX0 11B0 1nX0 Rd0 1zb0 Op0 1zb0 Op0 1zb0 Rd0 1zb0 Op0 1zb0 Op0 1zb0 Op0 1zb0 Op0 1zb0 Op0 1zb0 Rd0 1zb0 Op0 1zb0 Op0 1zb0 Op0 1zb0 Op0 1zb0 Rd0 1zb0 Op0 1zb0 Op0 1zb0 Op0 1zb0 Op0 1zb0 Op0 1zb0 Rd0 1zb0 Op0 1zb0 Op0 1zb0 Op0 1zb0 Op0 1zb0 Rd0 1zb0 Op0 1zb0 Op0 1zb0 Op0 1zb0 Op0 1zb0 Op0 1zb0|656');
var _elm_community$elm_time$Time_TimeZoneData$america_thunder_bay_l = _elm_community$elm_time$Time_TimeZoneData$unpack('America/Thunder_Bay|CST EST EWT EPT EDT|60 50 40 40 40|0123141414141414141414141414141414141414141414141414141414141414141414141414141414141414141414141414141414141414141414141414141414141414141|-2q5S0 1iaN0 8x40 iv0 XNB0 1cL0 1cN0 1fz0 1cN0 1cL0 3Cp0 1cL0 1cN0 1cL0 1cN0 1fz0 1a10 1fz0 1cN0 1cL0 1cN0 1cL0 1cN0 1cL0 1cN0 1cL0 1cN0 1fz0 1a10 1fz0 1cN0 1cL0 1cN0 1cL0 1cN0 1cL0 14p0 1lb0 14p0 1nX0 11B0 1nX0 11B0 1nX0 14p0 1lb0 14p0 1lb0 14p0 1nX0 11B0 1nX0 11B0 1nX0 14p0 1lb0 14p0 1lb0 14p0 1lb0 14p0 1nX0 11B0 1nX0 11B0 1nX0 14p0 1lb0 14p0 1lb0 14p0 1nX0 11B0 1nX0 11B0 1nX0 Rd0 1zb0 Op0 1zb0 Op0 1zb0 Rd0 1zb0 Op0 1zb0 Op0 1zb0 Op0 1zb0 Op0 1zb0 Op0 1zb0 Rd0 1zb0 Op0 1zb0 Op0 1zb0 Op0 1zb0 Op0 1zb0 Rd0 1zb0 Op0 1zb0 Op0 1zb0 Op0 1zb0 Op0 1zb0 Op0 1zb0 Rd0 1zb0 Op0 1zb0 Op0 1zb0 Op0 1zb0 Op0 1zb0 Rd0 1zb0 Op0 1zb0 Op0 1zb0 Op0 1zb0 Op0 1zb0 Op0 1zb0|11e4');
var _elm_community$elm_time$Time_TimeZoneData$america_tijuana_l = _elm_community$elm_time$Time_TimeZoneData$unpack('America/Tijuana|LMT MST PST PDT PWT PPT|7M.4 70 80 70 70 70|012123245232323232323232323232323232323232323232323232323232323232323232323232323232323232323232323232323232323232323232323232323232323232323232323232|-1UQE0 4PX0 8mM0 8lc0 SN0 1cL0 pHB0 83r0 zI0 5O10 1Rz0 cOO0 11A0 1o00 11A0 1qM0 11A0 1o00 11A0 1o00 11A0 1o00 11A0 1o00 11A0 BUp0 1fz0 1a10 1fz0 1cN0 1cL0 1cN0 1cL0 1cN0 1cL0 1cN0 1cL0 1cN0 1fz0 1a10 1fz0 1cN0 1cL0 1cN0 1cL0 1cN0 1cL0 14p0 1lb0 14p0 1nX0 11B0 1nX0 11B0 1nX0 14p0 1lb0 14p0 1lb0 14p0 1nX0 11B0 1nX0 11B0 1nX0 14p0 1lb0 14p0 1lb0 14p0 1lb0 14p0 1nX0 11B0 1nX0 11B0 1nX0 14p0 1lb0 14p0 1lb0 14p0 1nX0 11B0 1nX0 11B0 1nX0 11B0 1nX0 14p0 1lb0 14p0 1lb0 U10 1zb0 Op0 1zb0 Op0 1zb0 Op0 1zb0 Op0 1zb0 Op0 1zb0 Rd0 1zb0 Op0 1zb0 Op0 1zb0 Op0 1zb0 Op0 1zb0 Rd0 1zb0 Op0 1zb0 Op0 1zb0 Op0 1zb0 Op0 1zb0 Op0 1zb0 Rd0 1zb0 Op0 1zb0 Op0 1zb0 Op0 1zb0 Op0 1zb0 Rd0 1zb0 Op0 1zb0 Op0 1zb0 Op0 1zb0 Op0 1zb0 Op0 1zb0|20e5');
var _elm_community$elm_time$Time_TimeZoneData$america_toronto_l = _elm_community$elm_time$Time_TimeZoneData$unpack('America/Toronto|EST EDT EWT EPT|50 40 40 40|01010101010101010101010101010101010101010101012301010101010101010101010101010101010101010101010101010101010101010101010101010101010101010101010101010101010101010101010101010101010101010101010101010101010101010101010101010101010101010|-25TR0 1in0 11Wu 1nzu 1fD0 WJ0 1wr0 Nb0 1Ap0 On0 1zd0 On0 1wp0 TX0 1tB0 TX0 1tB0 TX0 1tB0 WL0 1qN0 11z0 1o10 11z0 1o10 11z0 1o10 11z0 1qN0 WL0 1qN0 11z0 1o10 11z0 1o10 11z0 1o10 11z0 1o10 11z0 1o10 11z0 1qN0 WL0 1qN0 4kM0 8x40 iv0 1o10 11z0 1nX0 11z0 1o10 11z0 1o10 1qL0 11D0 1nX0 11B0 11z0 1o10 11z0 1o10 11z0 1o10 11z0 1o10 11z0 1qN0 11z0 1o10 1cL0 1cN0 1cL0 1cN0 1cL0 1cN0 1fz0 1cN0 1cL0 1cN0 1cL0 1cN0 1cL0 1cN0 1cL0 1cN0 1fz0 1a10 1fz0 1cN0 1cL0 1cN0 1cL0 1cN0 1cL0 1cN0 1cL0 1cN0 1fz0 1cN0 1cL0 1cN0 1cL0 1cN0 1cL0 1cN0 1cL0 1cN0 1fz0 1a10 1fz0 1cN0 1cL0 1cN0 1cL0 1cN0 1cL0 1cN0 1cL0 1cN0 1fz0 1a10 1fz0 1cN0 1cL0 1cN0 1cL0 1cN0 1cL0 14p0 1lb0 14p0 1nX0 11B0 1nX0 11B0 1nX0 14p0 1lb0 14p0 1lb0 14p0 1nX0 11B0 1nX0 11B0 1nX0 14p0 1lb0 14p0 1lb0 14p0 1lb0 14p0 1nX0 11B0 1nX0 11B0 1nX0 14p0 1lb0 14p0 1lb0 14p0 1nX0 11B0 1nX0 11B0 1nX0 Rd0 1zb0 Op0 1zb0 Op0 1zb0 Rd0 1zb0 Op0 1zb0 Op0 1zb0 Op0 1zb0 Op0 1zb0 Op0 1zb0 Rd0 1zb0 Op0 1zb0 Op0 1zb0 Op0 1zb0 Op0 1zb0 Rd0 1zb0 Op0 1zb0 Op0 1zb0 Op0 1zb0 Op0 1zb0 Op0 1zb0 Rd0 1zb0 Op0 1zb0 Op0 1zb0 Op0 1zb0 Op0 1zb0 Rd0 1zb0 Op0 1zb0 Op0 1zb0 Op0 1zb0 Op0 1zb0 Op0 1zb0|65e5');
var _elm_community$elm_time$Time_TimeZoneData$america_vancouver_l = _elm_community$elm_time$Time_TimeZoneData$unpack('America/Vancouver|PST PDT PWT PPT|80 70 70 70|0102301010101010101010101010101010101010101010101010101010101010101010101010101010101010101010101010101010101010101010101010101010101010101010101010101010101010101010101010101010101010101010|-25TO0 1in0 UGp0 8x10 iy0 1o10 17b0 1ip0 11z0 1o10 11z0 1o10 11z0 1qN0 WL0 1qN0 11z0 1o10 11z0 1o10 11z0 1o10 11z0 1o10 11z0 1qN0 11z0 1o10 11z0 1o10 11z0 1o10 11z0 1o10 11z0 1qN0 WL0 1qN0 1cL0 1cN0 1cL0 1cN0 1cL0 1cN0 1fz0 1a10 1fz0 1cN0 1cL0 1cN0 1cL0 1cN0 1cL0 1cN0 1cL0 1cN0 1fz0 1cN0 1cL0 1cN0 1cL0 1cN0 1cL0 1cN0 1cL0 1cN0 1fz0 1a10 1fz0 1cN0 1cL0 1cN0 1cL0 1cN0 1cL0 1cN0 1cL0 1cN0 1fz0 1a10 1fz0 1cN0 1cL0 1cN0 1cL0 1cN0 1cL0 14p0 1lb0 14p0 1nX0 11B0 1nX0 11B0 1nX0 14p0 1lb0 14p0 1lb0 14p0 1nX0 11B0 1nX0 11B0 1nX0 14p0 1lb0 14p0 1lb0 14p0 1lb0 14p0 1nX0 11B0 1nX0 11B0 1nX0 14p0 1lb0 14p0 1lb0 14p0 1nX0 11B0 1nX0 11B0 1nX0 Rd0 1zb0 Op0 1zb0 Op0 1zb0 Rd0 1zb0 Op0 1zb0 Op0 1zb0 Op0 1zb0 Op0 1zb0 Op0 1zb0 Rd0 1zb0 Op0 1zb0 Op0 1zb0 Op0 1zb0 Op0 1zb0 Rd0 1zb0 Op0 1zb0 Op0 1zb0 Op0 1zb0 Op0 1zb0 Op0 1zb0 Rd0 1zb0 Op0 1zb0 Op0 1zb0 Op0 1zb0 Op0 1zb0 Rd0 1zb0 Op0 1zb0 Op0 1zb0 Op0 1zb0 Op0 1zb0 Op0 1zb0|23e5');
var _elm_community$elm_time$Time_TimeZoneData$america_whitehorse_l = _elm_community$elm_time$Time_TimeZoneData$unpack('America/Whitehorse|YST YDT YWT YPT YDDT PST PDT|90 80 80 80 70 80 70|0101023040565656565656565656565656565656565656565656565656565656565656565656565656565656565656565656565656565656565656565656565|-25TN0 1in0 1o10 13V0 Ser0 8x00 iz0 LCL0 1fA0 3NA0 vrd0 1cL0 1cN0 1cL0 1cN0 1fz0 1a10 1fz0 1cN0 1cL0 1cN0 1cL0 1cN0 1cL0 14p0 1lb0 14p0 1nX0 11B0 1nX0 11B0 1nX0 14p0 1lb0 14p0 1lb0 14p0 1nX0 11B0 1nX0 11B0 1nX0 14p0 1lb0 14p0 1lb0 14p0 1lb0 14p0 1nX0 11B0 1nX0 11B0 1nX0 14p0 1lb0 14p0 1lb0 14p0 1nX0 11B0 1nX0 11B0 1nX0 Rd0 1zb0 Op0 1zb0 Op0 1zb0 Rd0 1zb0 Op0 1zb0 Op0 1zb0 Op0 1zb0 Op0 1zb0 Op0 1zb0 Rd0 1zb0 Op0 1zb0 Op0 1zb0 Op0 1zb0 Op0 1zb0 Rd0 1zb0 Op0 1zb0 Op0 1zb0 Op0 1zb0 Op0 1zb0 Op0 1zb0 Rd0 1zb0 Op0 1zb0 Op0 1zb0 Op0 1zb0 Op0 1zb0 Rd0 1zb0 Op0 1zb0 Op0 1zb0 Op0 1zb0 Op0 1zb0 Op0 1zb0|23e3');
var _elm_community$elm_time$Time_TimeZoneData$america_winnipeg_l = _elm_community$elm_time$Time_TimeZoneData$unpack('America/Winnipeg|CST CDT CWT CPT|60 50 50 50|010101023010101010101010101010101010101010101010101010101010101010101010101010101010101010101010101010101010101010101010101010101010101010101010101010101010101010101010101010101010101010|-2aIi0 WL0 3ND0 1in0 Jap0 Rb0 aCN0 8x30 iw0 1tB0 11z0 1ip0 11z0 1o10 11z0 1o10 11z0 1rd0 10L0 1op0 11z0 1o10 11z0 1o10 11z0 1o10 11z0 1o10 11z0 1qN0 11z0 1o10 11z0 1o10 11z0 1o10 1cL0 1cN0 11z0 6i10 WL0 6i10 1fA0 1cM0 1cM0 1cM0 1cM0 1cM0 1cM0 1cM0 1cM0 1cM0 1fA0 1cM0 1cM0 1cM0 1cM0 1cM0 1cM0 1cM0 1cM0 1cM0 1fA0 1a00 1fA0 1cM0 1cM0 1cM0 1cM0 1cM0 1cM0 1cM0 1cM0 1cM0 1fA0 1a00 1fA0 1cM0 1cM0 1cM0 1cM0 1cM0 1cM0 14o0 1lc0 14o0 1o00 11A0 1o00 11A0 1o00 14o0 1lc0 14o0 1lc0 14o0 1o00 11A0 1o00 11A0 1o00 14o0 1lc0 14o0 1lc0 14o0 1lc0 14o0 1o00 11A0 1o00 11A0 1o00 14o0 1lc0 14o0 1lc0 14o0 1o00 11A0 1o00 11A0 1nX0 Rd0 1zb0 Op0 1zb0 Op0 1zb0 Rd0 1zb0 Op0 1zb0 Op0 1zb0 Op0 1zb0 Op0 1zb0 Op0 1zb0 Rd0 1zb0 Op0 1zb0 Op0 1zb0 Op0 1zb0 Op0 1zb0 Rd0 1zb0 Op0 1zb0 Op0 1zb0 Op0 1zb0 Op0 1zb0 Op0 1zb0 Rd0 1zb0 Op0 1zb0 Op0 1zb0 Op0 1zb0 Op0 1zb0 Rd0 1zb0 Op0 1zb0 Op0 1zb0 Op0 1zb0 Op0 1zb0 Op0 1zb0|66e4');
var _elm_community$elm_time$Time_TimeZoneData$america_yakutat_l = _elm_community$elm_time$Time_TimeZoneData$unpack('America/Yakutat|YST YWT YPT YDT AKST AKDT|90 80 80 80 90 80|01203030303030303030303030303030304545454545454545454545454545454545454545454545454545454545454545454545454545454545454545454545454545454545454|-17T10 8x00 iz0 Vo10 1cL0 1cN0 1cL0 1cN0 1fz0 1cN0 1cL0 1cN0 1cL0 s10 1Vz0 LB0 1BX0 1cN0 1fz0 1a10 1fz0 1cN0 1cL0 1cN0 1cL0 1cN0 1cL0 1cN0 1cL0 1cN0 1fz0 1a10 1fz0 cn0 10q0 1cL0 1cN0 1cL0 1cN0 1cL0 14p0 1lb0 14p0 1nX0 11B0 1nX0 11B0 1nX0 14p0 1lb0 14p0 1lb0 14p0 1nX0 11B0 1nX0 11B0 1nX0 14p0 1lb0 14p0 1lb0 14p0 1lb0 14p0 1nX0 11B0 1nX0 11B0 1nX0 14p0 1lb0 14p0 1lb0 14p0 1nX0 11B0 1nX0 11B0 1nX0 Rd0 1zb0 Op0 1zb0 Op0 1zb0 Rd0 1zb0 Op0 1zb0 Op0 1zb0 Op0 1zb0 Op0 1zb0 Op0 1zb0 Rd0 1zb0 Op0 1zb0 Op0 1zb0 Op0 1zb0 Op0 1zb0 Rd0 1zb0 Op0 1zb0 Op0 1zb0 Op0 1zb0 Op0 1zb0 Op0 1zb0 Rd0 1zb0 Op0 1zb0 Op0 1zb0 Op0 1zb0 Op0 1zb0 Rd0 1zb0 Op0 1zb0 Op0 1zb0 Op0 1zb0 Op0 1zb0 Op0 1zb0|642');
var _elm_community$elm_time$Time_TimeZoneData$america_yellowknife_l = _elm_community$elm_time$Time_TimeZoneData$unpack('America/Yellowknife|-00 MST MWT MPT MDDT MDT|0 70 60 60 50 60|012314151515151515151515151515151515151515151515151515151515151515151515151515151515151515151515151515151515151515151515151|-1pdA0 hix0 8x20 ix0 LCL0 1fA0 zgO0 1cL0 1cN0 1cL0 1cN0 1fz0 1a10 1fz0 1cN0 1cL0 1cN0 1cL0 1cN0 1cL0 14p0 1lb0 14p0 1nX0 11B0 1nX0 11B0 1nX0 14p0 1lb0 14p0 1lb0 14p0 1nX0 11B0 1nX0 11B0 1nX0 14p0 1lb0 14p0 1lb0 14p0 1lb0 14p0 1nX0 11B0 1nX0 11B0 1nX0 14p0 1lb0 14p0 1lb0 14p0 1nX0 11B0 1nX0 11B0 1nX0 Rd0 1zb0 Op0 1zb0 Op0 1zb0 Rd0 1zb0 Op0 1zb0 Op0 1zb0 Op0 1zb0 Op0 1zb0 Op0 1zb0 Rd0 1zb0 Op0 1zb0 Op0 1zb0 Op0 1zb0 Op0 1zb0 Rd0 1zb0 Op0 1zb0 Op0 1zb0 Op0 1zb0 Op0 1zb0 Op0 1zb0 Rd0 1zb0 Op0 1zb0 Op0 1zb0 Op0 1zb0 Op0 1zb0 Rd0 1zb0 Op0 1zb0 Op0 1zb0 Op0 1zb0 Op0 1zb0 Op0 1zb0|19e3');
var _elm_community$elm_time$Time_TimeZoneData$antarctica_casey_l = _elm_community$elm_time$Time_TimeZoneData$unpack('Antarctica/Casey|-00 +08 +11|0 -80 -b0|0121212|-2q00 1DjS0 T90 40P0 KL0 blz0|10');
var _elm_community$elm_time$Time_TimeZoneData$antarctica_davis_l = _elm_community$elm_time$Time_TimeZoneData$unpack('Antarctica/Davis|-00 +07 +05|0 -70 -50|01012121|-vyo0 iXt0 alj0 1D7v0 VB0 3Wn0 KN0|70');
var _elm_community$elm_time$Time_TimeZoneData$antarctica_dumontdurville_l = _elm_community$elm_time$Time_TimeZoneData$unpack('Antarctica/DumontDUrville|-00 +10|0 -a0|0101|-U0o0 cfq0 bFm0|80');
var _elm_community$elm_time$Time_TimeZoneData$antarctica_macquarie_l = _elm_community$elm_time$Time_TimeZoneData$unpack('Antarctica/Macquarie|AEST AEDT -00 MIST|-a0 -b0 0 -b0|0102010101010101010101010101010101010101010101010101010101010101010101010101010101010101013|-29E80 19X0 4SL0 1ayy0 Lvs0 1cM0 1o00 Rc0 1wo0 Rc0 1wo0 U00 1wo0 LA0 1C00 Oo0 1zc0 Oo0 1zc0 Oo0 1zc0 Rc0 1zc0 Oo0 1zc0 Oo0 1zc0 Oo0 1zc0 Oo0 1zc0 Oo0 1zc0 11A0 1qM0 WM0 1qM0 Oo0 1zc0 Oo0 1zc0 Oo0 1wo0 WM0 1tA0 WM0 1tA0 U00 1tA0 U00 1tA0 11A0 1fA0 1a00 1fA0 1a00 1fA0 1a00 1fA0 1a00 1fA0 1cM0 1fA0 1a00 1fA0 1a00 1fA0 1a00 1fA0 1a00 11A0 1o00 1io0 1a00 1fA0 1a00 1fA0 1a00 1fA0 1a00 1fA0 1cM0 1cM0 1a00 1io0 1cM0 1cM0 1cM0 1cM0 1cM0|1');
var _elm_community$elm_time$Time_TimeZoneData$antarctica_mawson_l = _elm_community$elm_time$Time_TimeZoneData$unpack('Antarctica/Mawson|-00 +06 +05|0 -60 -50|012|-CEo0 2fyk0|60');
var _elm_community$elm_time$Time_TimeZoneData$antarctica_palmer_l = _elm_community$elm_time$Time_TimeZoneData$unpack('Antarctica/Palmer|-00 ARST ART ART ARST CLT CLST|0 30 40 30 20 40 30|0121212121234356565656565656565656565656565656565656565656565656565656565656565656565656565656565656565656565656565656565656|-cao0 nD0 1vd0 SL0 1vd0 17z0 1cN0 1fz0 1cN0 1cL0 1cN0 asn0 Db0 jsN0 14N0 11z0 1o10 11z0 1qN0 WL0 1qN0 WL0 1qN0 1cL0 1cN0 11z0 1o10 11z0 1qN0 WL0 1fB0 19X0 1qN0 11z0 1o10 11z0 1o10 11z0 1o10 11z0 1qN0 WL0 1qN0 17b0 1ip0 11z0 1ip0 1fz0 1fB0 11z0 1qN0 WL0 1qN0 WL0 1qN0 WL0 1qN0 11z0 1o10 11z0 1o10 11z0 1qN0 WL0 1qN0 17b0 1ip0 11z0 1o10 19X0 1fB0 1nX0 G10 1EL0 Op0 1zb0 Rd0 1wn0 Rd0 46n0 Ap0 1Nb0 Ap0 1Nb0 Ap0 1Nb0 Ap0 1Nb0 Ap0 1Nb0 Dd0 1Nb0 Ap0 1Nb0 Ap0 1Nb0 Ap0 1Nb0 Ap0 1Nb0 Ap0 1Nb0 Dd0 1Nb0 Ap0 1Nb0 Ap0 1Nb0 Ap0 1Nb0 Ap0 1Nb0 Dd0 1Nb0 Ap0 1Nb0 Ap0 1Nb0 Ap0 1Nb0 Ap0 1Nb0 Ap0|40');
var _elm_community$elm_time$Time_TimeZoneData$antarctica_rothera_l = _elm_community$elm_time$Time_TimeZoneData$unpack('Antarctica/Rothera|-00 -03|0 30|01|gOo0|130');
var _elm_community$elm_time$Time_TimeZoneData$antarctica_syowa_l = _elm_community$elm_time$Time_TimeZoneData$unpack('Antarctica/Syowa|-00 +03|0 -30|01|-vs00|20');
var _elm_community$elm_time$Time_TimeZoneData$antarctica_troll_l = _elm_community$elm_time$Time_TimeZoneData$unpack('Antarctica/Troll|-00 +00 +02|0 0 -20|01212121212121212121212121212121212121212121212121212121212121212121|1puo0 hd0 1qM0 WM0 1qM0 WM0 1qM0 11A0 1o00 11A0 1o00 11A0 1qM0 WM0 1qM0 WM0 1qM0 11A0 1o00 11A0 1o00 11A0 1o00 11A0 1qM0 WM0 1qM0 WM0 1qM0 11A0 1o00 11A0 1o00 11A0 1qM0 WM0 1qM0 WM0 1qM0 11A0 1o00 11A0 1o00 11A0 1o00 11A0 1qM0 WM0 1qM0 WM0 1qM0 11A0 1o00 11A0 1o00 11A0 1qM0 WM0 1qM0 WM0 1qM0 WM0 1qM0 11A0 1o00 11A0 1o00|40');
var _elm_community$elm_time$Time_TimeZoneData$antarctica_vostok_l = _elm_community$elm_time$Time_TimeZoneData$unpack('Antarctica/Vostok|-00 +06|0 -60|01|-tjA0|25');
var _elm_community$elm_time$Time_TimeZoneData$asia_almaty_l = _elm_community$elm_time$Time_TimeZoneData$unpack('Asia/Almaty|LMT +05 +06 +07|-57.M -50 -60 -70|012323232323232323232321232323232323232323232323232|-1Pc57.M eUo7.M 23CL0 1db0 1cN0 1db0 1cN0 1db0 1dd0 1cO0 1cM0 1cM0 1cM0 1cM0 1cM0 1cM0 1cM0 1cM0 1cM0 1cM0 1cM0 1fA0 2pB0 IM0 rX0 1cM0 1cM0 1cM0 1cM0 1cM0 1cM0 1cM0 1fA0 1o00 11A0 1o00 11A0 1o00 11A0 1qM0 WM0 1qM0 WM0 1qM0 11A0 1o00 11A0 1o00 11A0 1qM0|15e5');
var _elm_community$elm_time$Time_TimeZoneData$asia_amman_l = _elm_community$elm_time$Time_TimeZoneData$unpack('Asia/Amman|LMT EET EEST|-2n.I -20 -30|0121212121212121212121212121212121212121212121212121212121212121212121212121212121212121212121212121212121212121212121|-1yW2n.I 1HiMn.I KL0 1oN0 11b0 1oN0 11b0 1pd0 1dz0 1cp0 11b0 1op0 11b0 fO10 1db0 1e10 1cL0 1cN0 1cL0 1cN0 1fz0 1pd0 10n0 1ld0 14n0 1hB0 15b0 1ip0 19X0 1cN0 1cL0 1cN0 17b0 1ld0 14o0 1lc0 17c0 1io0 17c0 1io0 17c0 1So0 y00 1fc0 1dc0 1co0 1dc0 1cM0 1cM0 1cM0 1o00 11A0 1lc0 17c0 1cM0 1cM0 1o00 11A0 1o00 11A0 1qM0 WM0 1qM0 WM0 1qM0 11A0 1o00 11A0 4bX0 Dd0 1qM0 WM0 1qM0 11A0 1o00 11A0 1o00 11A0 1o00 11A0 1o00 11A0 1qM0 WM0 1qM0 11A0 1o00 11A0 1o00 11A0 1o00 11A0 1qM0 WM0 1qM0 WM0 1qM0 11A0 1o00 11A0 1o00 11A0 1o00 11A0 1qM0 WM0 1qM0 11A0 1o00 11A0 1o00 11A0 1o00 11A0 1qM0 WM0 1qM0|25e5');
var _elm_community$elm_time$Time_TimeZoneData$asia_anadyr_l = _elm_community$elm_time$Time_TimeZoneData$unpack('Asia/Anadyr|LMT +12 +13 +14 +11|-bN.U -c0 -d0 -e0 -b0|01232121212121212121214121212121212121212121212121212121212141|-1PcbN.U eUnN.U 23CL0 1db0 2q10 1cN0 1db0 1dd0 1cO0 1cM0 1cM0 1cM0 1cM0 1cM0 1cM0 1cM0 1cM0 1cM0 1cM0 1cM0 1fA0 2pB0 IM0 rX0 1cM0 1cM0 1cM0 1cM0 1cM0 1cM0 1cM0 1fA0 1o00 11A0 1o00 11A0 1o00 11A0 1qM0 WM0 1qM0 WM0 1qM0 11A0 1o00 11A0 1o00 11A0 1qM0 WM0 1qM0 WM0 1qM0 WM0 1qM0 11A0 1o00 11A0 1o00 2sp0 WM0|13e3');
var _elm_community$elm_time$Time_TimeZoneData$asia_aqtau_l = _elm_community$elm_time$Time_TimeZoneData$unpack('Asia/Aqtau|LMT +04 +05 +06|-3l.4 -40 -50 -60|012323232323232323232123232312121212121212121212|-1Pc3l.4 eUnl.4 24PX0 2pX0 1cN0 1db0 1dd0 1cO0 1cM0 1cM0 1cM0 1cM0 1cM0 1cM0 1cM0 1cM0 1cM0 1cM0 1cM0 1fA0 2pB0 IM0 rX0 1cM0 1cM0 1cM0 1cM0 1cM0 1cN0 1cM0 1fA0 1o00 11A0 1o00 11A0 1o00 11A0 1qM0 WM0 1qM0 WM0 1qM0 11A0 1o00 11A0 1o00 11A0|15e4');
var _elm_community$elm_time$Time_TimeZoneData$asia_aqtobe_l = _elm_community$elm_time$Time_TimeZoneData$unpack('Asia/Aqtobe|LMT +04 +05 +06|-3M.E -40 -50 -60|0123232323232323232321232323232323232323232323232|-1Pc3M.E eUnM.E 23CL0 3Db0 1cN0 1db0 1dd0 1cO0 1cM0 1cM0 1cM0 1cM0 1cM0 1cM0 1cM0 1cM0 1cM0 1cM0 1cM0 1fA0 2pB0 IM0 rX0 1cM0 1cM0 1cM0 1cM0 1cM0 1cM0 1cM0 1fA0 1o00 11A0 1o00 11A0 1o00 11A0 1qM0 WM0 1qM0 WM0 1qM0 11A0 1o00 11A0 1o00 11A0 1qM0|27e4');
var _elm_community$elm_time$Time_TimeZoneData$asia_ashgabat_l = _elm_community$elm_time$Time_TimeZoneData$unpack('Asia/Ashgabat|LMT +04 +05 +06|-3R.w -40 -50 -60|0123232323232323232323212|-1Pc3R.w eUnR.w 23CL0 1db0 1cN0 1db0 1cN0 1db0 1dd0 1cO0 1cM0 1cM0 1cM0 1cM0 1cM0 1cM0 1cM0 1cM0 1cM0 1cM0 1cM0 1fA0 2pB0 IM0|41e4');
var _elm_community$elm_time$Time_TimeZoneData$asia_atyrau_l = _elm_community$elm_time$Time_TimeZoneData$unpack('Asia/Atyrau|LMT +04 +05 +06|-3r.I -40 -50 -60|01232323232323232323212323232323232321212121212|-1Pc3r.I eUnr.I 24PX0 2pX0 1cN0 1db0 1dd0 1cO0 1cM0 1cM0 1cM0 1cM0 1cM0 1cM0 1cM0 1cM0 1cM0 1cM0 1cM0 1fA0 2pB0 IM0 rX0 1cM0 1cM0 1cM0 1cM0 1cM0 1cM0 1cM0 1fA0 1o00 11A0 1o00 11A0 1o00 2sp0 WM0 1qM0 WM0 1qM0 11A0 1o00 11A0 1o00 11A0');
var _elm_community$elm_time$Time_TimeZoneData$asia_baghdad_l = _elm_community$elm_time$Time_TimeZoneData$unpack('Asia/Baghdad|BMT AST ADT|-2V.A -30 -40|012121212121212121212121212121212121212121212121212121|-26BeV.A 2ACnV.A 11b0 1cp0 1dz0 1dd0 1db0 1cN0 1cp0 1cM0 1cM0 1cM0 1cM0 1cM0 1cM0 1cM0 1cM0 1cM0 1fA0 1de0 1dc0 1dc0 1dc0 1cM0 1dc0 1cM0 1dc0 1cM0 1dc0 1dc0 1dc0 1cM0 1dc0 1cM0 1dc0 1cM0 1dc0 1dc0 1dc0 1cM0 1dc0 1cM0 1dc0 1cM0 1dc0 1dc0 1dc0 1cM0 1dc0 1cM0 1dc0 1cM0 1dc0|66e5');
var _elm_community$elm_time$Time_TimeZoneData$asia_baku_l = _elm_community$elm_time$Time_TimeZoneData$unpack('Asia/Baku|LMT +03 +04 +05|-3j.o -30 -40 -50|01232323232323232323232123232323232323232323232323232323232323232|-1Pc3j.o 1jUoj.o WCL0 1db0 1cN0 1db0 1cN0 1db0 1dd0 1cO0 1cM0 1cM0 1cM0 1cM0 1cM0 1cM0 1cM0 1cM0 1cM0 1cM0 1cM0 1fA0 2pB0 1cM0 9Je0 1o00 11z0 1o00 11A0 1o00 11A0 1qM0 WM0 1qM0 WM0 1qM0 11A0 1o00 11A0 1o00 11A0 1qM0 WM0 1qM0 WM0 1qM0 WM0 1qM0 11A0 1o00 11A0 1o00 11A0 1qM0 WM0 1qM0 WM0 1qM0 11A0 1o00 11A0 1o00 11A0 1o00|27e5');
var _elm_community$elm_time$Time_TimeZoneData$asia_bangkok_l = _elm_community$elm_time$Time_TimeZoneData$unpack('Asia/Bangkok|BMT ICT|-6G.4 -70|01|-218SG.4|15e6');
var _elm_community$elm_time$Time_TimeZoneData$asia_barnaul_l = _elm_community$elm_time$Time_TimeZoneData$unpack('Asia/Barnaul|LMT +06 +07 +08|-5z -60 -70 -80|0123232323232323232323212323232321212121212121212121212121212121212|-21S5z pCnz 23CL0 1db0 1cN0 1db0 1cN0 1db0 1dd0 1cO0 1cM0 1cM0 1cM0 1cM0 1cM0 1cM0 1cM0 1cM0 1cM0 1cM0 1cM0 1fA0 2pB0 IM0 rX0 1cM0 1cM0 1cM0 1cM0 1cM0 1cM0 p90 LE0 1fA0 1o00 11A0 1o00 11A0 1o00 11A0 1qM0 WM0 1qM0 WM0 1qM0 11A0 1o00 11A0 1o00 11A0 1qM0 WM0 1qM0 WM0 1qM0 WM0 1qM0 11A0 1o00 11A0 1o00 11A0 1qM0 WM0 8Hz0 3rd0');
var _elm_community$elm_time$Time_TimeZoneData$asia_beirut_l = _elm_community$elm_time$Time_TimeZoneData$unpack('Asia/Beirut|EET EEST|-20 -30|010101010101010101010101010101010101010101010101010101010101010101010101010101010101010101010101010101010101010101010101010101010101010101010|-21aq0 1on0 1410 1db0 19B0 1in0 1ip0 WL0 1lQp0 11b0 1oN0 11b0 1oN0 11b0 1pd0 11b0 1oN0 11b0 q6N0 En0 1oN0 11b0 1oN0 11b0 1oN0 11b0 1pd0 11b0 1oN0 11b0 1op0 11b0 dA10 17b0 1iN0 17b0 1iN0 17b0 1iN0 17b0 1vB0 SL0 1mp0 13z0 1iN0 17b0 1iN0 17b0 1jd0 12n0 1a10 1cL0 1cN0 1cL0 1cN0 1cL0 1fB0 1cL0 1cN0 1cL0 1cN0 1cL0 1cN0 1qL0 WN0 1qL0 WN0 1qL0 11B0 1nX0 11B0 1nX0 11B0 1qL0 WN0 1qL0 WN0 1qL0 WN0 1qL0 11B0 1nX0 11B0 1nX0 11B0 1qL0 WN0 1qL0 WN0 1qL0 11B0 1nX0 11B0 1nX0 11B0 1nX0 11B0 1qL0 WN0 1qL0 WN0 1qL0 11B0 1nX0 11B0 1nX0 11B0 1qL0 WN0 1qL0 WN0 1qL0 11B0 1nX0 11B0 1nX0 11B0 1nX0 11B0 1qL0 WN0 1qL0 WN0 1qL0 11B0 1nX0 11B0 1nX0 11B0 1qL0 WN0 1qL0 WN0 1qL0 WN0 1qL0 11B0 1nX0 11B0 1nX0|22e5');
var _elm_community$elm_time$Time_TimeZoneData$asia_bishkek_l = _elm_community$elm_time$Time_TimeZoneData$unpack('Asia/Bishkek|LMT +05 +06 +07|-4W.o -50 -60 -70|012323232323232323232321212121212121212121212121212|-1Pc4W.o eUnW.o 23CL0 1db0 1cN0 1db0 1cN0 1db0 1dd0 1cO0 1cM0 1cM0 1cM0 1cM0 1cM0 1cM0 1cM0 1cM0 1cM0 1cM0 1cM0 1fA0 2e00 1tX0 17b0 1ip0 17b0 1ip0 17b0 1ip0 17b0 1ip0 19X0 1cPu 1nX0 11B0 1nX0 11B0 1qL0 WN0 1qL0 WN0 1qL0 11B0 1nX0 11B0 1nX0 11B0 1qL0 WN0|87e4');
var _elm_community$elm_time$Time_TimeZoneData$asia_brunei_l = _elm_community$elm_time$Time_TimeZoneData$unpack('Asia/Brunei|LMT BNT BNT|-7D.E -7u -80|012|-1KITD.E gDc9.E|42e4');
var _elm_community$elm_time$Time_TimeZoneData$asia_chita_l = _elm_community$elm_time$Time_TimeZoneData$unpack('Asia/Chita|LMT +08 +09 +10|-7x.Q -80 -90 -a0|012323232323232323232321232323232323232323232323232323232323232312|-21Q7x.Q pAnx.Q 23CL0 1db0 1cN0 1db0 1cN0 1db0 1dd0 1cO0 1cM0 1cM0 1cM0 1cM0 1cM0 1cM0 1cM0 1cM0 1cM0 1cM0 1cM0 1fA0 2pB0 IM0 rX0 1cM0 1cM0 1cM0 1cM0 1cM0 1cM0 1cM0 1fA0 1o00 11A0 1o00 11A0 1o00 11A0 1qM0 WM0 1qM0 WM0 1qM0 11A0 1o00 11A0 1o00 11A0 1qM0 WM0 1qM0 WM0 1qM0 WM0 1qM0 11A0 1o00 11A0 1o00 11A0 1qM0 WM0 8Hz0 3re0|33e4');
var _elm_community$elm_time$Time_TimeZoneData$asia_choibalsan_l = _elm_community$elm_time$Time_TimeZoneData$unpack('Asia/Choibalsan|LMT ULAT ULAT CHOST CHOT CHOT CHOST|-7C -70 -80 -a0 -90 -80 -90|0123434343434343434343434343434343434343434343456565656565656565656565656565656565656565656565|-2APHC 2UkoC cKn0 1da0 1dd0 1cL0 1cN0 1cL0 1cN0 1cL0 1cN0 1cL0 1cN0 1cL0 1cN0 1cL0 1cN0 1fz0 1cN0 1cL0 1cN0 1cL0 1cN0 1cL0 1cN0 1cL0 1cN0 1cL0 1fB0 1cL0 1cN0 1cL0 1cN0 1cL0 6hD0 11z0 1cN0 1cL0 1cN0 1cL0 1cN0 1cL0 1cN0 1cL0 1cN0 1fz0 3Db0 h1f0 1cJ0 1cP0 1cJ0 1cP0 1fx0 1cP0 1cJ0 1cP0 1cJ0 1cP0 1cJ0 1cP0 1cJ0 1cP0 1cJ0 1cP0 1fx0 1cP0 1cJ0 1cP0 1cJ0 1cP0 1cJ0 1cP0 1cJ0 1cP0 1fx0 1cP0 1cJ0 1cP0 1cJ0 1cP0 1cJ0 1cP0 1cJ0 1cP0 1cJ0 1cP0 1fx0 1cP0 1cJ0 1cP0 1cJ0 1cP0 1cJ0|38e3');
var _elm_community$elm_time$Time_TimeZoneData$asia_colombo_l = _elm_community$elm_time$Time_TimeZoneData$unpack('Asia/Colombo|MMT +0530 +06 +0630|-5j.w -5u -60 -6u|01231321|-2zOtj.w 1rFbN.w 1zzu 7Apu 23dz0 11zu n3cu|22e5');
var _elm_community$elm_time$Time_TimeZoneData$asia_damascus_l = _elm_community$elm_time$Time_TimeZoneData$unpack('Asia/Damascus|LMT EET EEST|-2p.c -20 -30|01212121212121212121212121212121212121212121212121212121212121212121212121212121212121212121212121212121212121212121212121212121212121212121212121212121|-21Jep.c Hep.c 17b0 1ip0 17b0 1ip0 17b0 1ip0 19X0 1xRB0 11X0 1oN0 10L0 1pB0 11b0 1oN0 10L0 1mp0 13X0 1oN0 11b0 1pd0 11b0 1oN0 11b0 1oN0 11b0 1oN0 11b0 1pd0 11b0 1oN0 11b0 1oN0 11b0 1oN0 11b0 1pd0 11b0 1oN0 Nb0 1AN0 Nb0 bcp0 19X0 1gp0 19X0 3ld0 1xX0 Vd0 1Bz0 Sp0 1vX0 10p0 1dz0 1cN0 1cL0 1db0 1db0 1g10 1an0 1ap0 1db0 1fd0 1db0 1cN0 1db0 1dd0 1db0 1cp0 1dz0 1c10 1dX0 1cN0 1db0 1dd0 1db0 1cN0 1db0 1cN0 1db0 1cN0 1db0 1dd0 1db0 1cN0 1db0 1cN0 19z0 1fB0 1qL0 11B0 1on0 Wp0 1qL0 11B0 1nX0 11B0 1nX0 11B0 1nX0 11B0 1nX0 11B0 1qL0 WN0 1qL0 WN0 1qL0 11B0 1nX0 11B0 1nX0 11B0 1nX0 11B0 1qL0 WN0 1qL0 WN0 1qL0 11B0 1nX0 11B0 1nX0 11B0 1qL0 WN0 1qL0 WN0 1qL0 11B0 1nX0 11B0 1nX0 11B0 1nX0 11B0 1qL0 WN0 1qL0 WN0 1qL0 11B0 1nX0 11B0 1nX0 11B0 1qL0 WN0 1qL0|26e5');
var _elm_community$elm_time$Time_TimeZoneData$asia_dhaka_l = _elm_community$elm_time$Time_TimeZoneData$unpack('Asia/Dhaka|HMT BURT IST DACT BDT BDST|-5R.k -6u -5u -60 -60 -70|01213454|-18LFR.k 1unn.k HB0 m6n0 LqMu 1x6n0 1i00|16e6');
var _elm_community$elm_time$Time_TimeZoneData$asia_dili_l = _elm_community$elm_time$Time_TimeZoneData$unpack('Asia/Dili|LMT TLT JST TLT WITA|-8m.k -80 -90 -90 -80|012343|-2le8m.k 1dnXm.k 8HA0 1ew00 Xld0|19e4');
var _elm_community$elm_time$Time_TimeZoneData$asia_dubai_l = _elm_community$elm_time$Time_TimeZoneData$unpack('Asia/Dubai|LMT GST|-3F.c -40|01|-21JfF.c|39e5');
var _elm_community$elm_time$Time_TimeZoneData$asia_dushanbe_l = _elm_community$elm_time$Time_TimeZoneData$unpack('Asia/Dushanbe|LMT +05 +06 +07|-4z.c -50 -60 -70|012323232323232323232321|-1Pc4z.c eUnz.c 23CL0 1db0 1cN0 1db0 1cN0 1db0 1dd0 1cO0 1cM0 1cM0 1cM0 1cM0 1cM0 1cM0 1cM0 1cM0 1cM0 1cM0 1cM0 1fA0 2hB0|76e4');
var _elm_community$elm_time$Time_TimeZoneData$asia_famagusta_l = _elm_community$elm_time$Time_TimeZoneData$unpack('Asia/Famagusta|LMT EET EEST +03|-2f.M -20 -30 -30|01212121212121212121212121212121212121212121212121212121212121212121212121212121212123|-1Vc2f.M 2a3cf.M 1cL0 1qp0 Xz0 19B0 19X0 1fB0 1db0 1cp0 1cL0 1fB0 19X0 1cN0 1cL0 1cN0 1cL0 1cN0 1cL0 1cN0 1fz0 1cN0 1cL0 1cN0 1cL0 1cN0 1cL0 1cN0 1cL0 1cN0 1cL0 1cN0 1fz0 1cN0 1cL0 1cN0 1cL0 1cN0 1cL0 1cN0 1cL0 1cN0 1cL0 1fB0 1cL0 1cN0 1cL0 1cN0 1o30 11A0 1qM0 WM0 1qM0 WM0 1qM0 11A0 1o00 11A0 1o00 11A0 1qM0 WM0 1qM0 WM0 1qM0 WM0 1qM0 11A0 1o00 11A0 1o00 11A0 1qM0 WM0 1qM0 WM0 1qM0 11A0 1o00 11A0 1o00 11A0 1o00 11A0 15U0');
var _elm_community$elm_time$Time_TimeZoneData$asia_gaza_l = _elm_community$elm_time$Time_TimeZoneData$unpack('Asia/Gaza|EET EEST IST IDT|-20 -30 -20 -30|010101010101010101010101010101012323232323232323232323232320101010101010101010101010101010101010101010101010101010101010101010101010101010101010|-1c2q0 5Rb0 10r0 1px0 10N0 1pz0 16p0 1jB0 16p0 1jx0 pBd0 Vz0 1oN0 11b0 1oO0 10N0 1pz0 10N0 1pb0 10N0 1pb0 10N0 1pb0 10N0 1pz0 10N0 1pb0 10N0 1pb0 11d0 1oL0 dW0 hfB0 Db0 1fB0 Rb0 npB0 11z0 1C10 IL0 1s10 10n0 1o10 WL0 1zd0 On0 1ld0 11z0 1o10 14n0 1o10 14n0 1nd0 12n0 1nd0 Xz0 1q10 12n0 M10 C00 17c0 1io0 17c0 1io0 17c0 1o00 1cL0 1fB0 1cL0 1cN0 1cL0 1cN0 1cL0 1cN0 1cL0 1cN0 17c0 1io0 18N0 1bz0 19z0 1gp0 1610 1iL0 11z0 1o10 14o0 1lA1 SKX 1xd1 MKX 1AN0 1a00 1fA0 1cL0 1cN0 1nX0 1210 1nz0 1220 1qL0 WN0 1qL0 11B0 1nX0 11B0 1nX0 11B0 1qL0 WN0 1qL0 WN0 1qL0 WN0 1qL0 11B0 1nX0 11B0 1nX0 11B0 1qL0 WN0 1qL0 WN0 1qL0 11B0 1nX0 11B0 1nX0 11B0 1nX0 11B0 1qL0 WN0 1qL0 WN0 1qL0 11B0 1nX0 11B0 1nX0 11B0 1qL0|18e5');
var _elm_community$elm_time$Time_TimeZoneData$asia_hebron_l = _elm_community$elm_time$Time_TimeZoneData$unpack('Asia/Hebron|EET EEST IST IDT|-20 -30 -20 -30|01010101010101010101010101010101232323232323232323232323232010101010101010101010101010101010101010101010101010101010101010101010101010101010101010|-1c2q0 5Rb0 10r0 1px0 10N0 1pz0 16p0 1jB0 16p0 1jx0 pBd0 Vz0 1oN0 11b0 1oO0 10N0 1pz0 10N0 1pb0 10N0 1pb0 10N0 1pb0 10N0 1pz0 10N0 1pb0 10N0 1pb0 11d0 1oL0 dW0 hfB0 Db0 1fB0 Rb0 npB0 11z0 1C10 IL0 1s10 10n0 1o10 WL0 1zd0 On0 1ld0 11z0 1o10 14n0 1o10 14n0 1nd0 12n0 1nd0 Xz0 1q10 12n0 M10 C00 17c0 1io0 17c0 1io0 17c0 1o00 1cL0 1fB0 1cL0 1cN0 1cL0 1cN0 1cL0 1cN0 1cL0 1cN0 17c0 1io0 18N0 1bz0 19z0 1gp0 1610 1iL0 12L0 1mN0 14o0 1lc0 Tb0 1xd1 MKX bB0 cn0 1cN0 1a00 1fA0 1cL0 1cN0 1nX0 1210 1nz0 1220 1qL0 WN0 1qL0 11B0 1nX0 11B0 1nX0 11B0 1qL0 WN0 1qL0 WN0 1qL0 WN0 1qL0 11B0 1nX0 11B0 1nX0 11B0 1qL0 WN0 1qL0 WN0 1qL0 11B0 1nX0 11B0 1nX0 11B0 1nX0 11B0 1qL0 WN0 1qL0 WN0 1qL0 11B0 1nX0 11B0 1nX0 11B0 1qL0|25e4');
var _elm_community$elm_time$Time_TimeZoneData$asia_ho_chi_minh_l = _elm_community$elm_time$Time_TimeZoneData$unpack('Asia/Ho_Chi_Minh|LMT PLMT ICT IDT JST|-76.E -76.u -70 -80 -90|0123423232|-2yC76.E bK00.a 1h7b6.u 5lz0 18o0 3Oq0 k5b0 aW00 BAM0|90e5');
var _elm_community$elm_time$Time_TimeZoneData$asia_hong_kong_l = _elm_community$elm_time$Time_TimeZoneData$unpack('Asia/Hong_Kong|LMT HKT HKST JST|-7A.G -80 -90 -90|0121312121212121212121212121212121212121212121212121212121212121212121|-2CFHA.G 1sEP6.G 1cL0 ylu 93X0 1qQu 1tX0 Rd0 1In0 NB0 1cL0 11B0 1nX0 11B0 1nX0 11B0 1nX0 14p0 1kL0 14N0 1nX0 U10 1tz0 U10 1wn0 Rd0 1wn0 U10 1tz0 U10 1tz0 U10 1tz0 U10 1wn0 Rd0 1wn0 Rd0 1wn0 U10 1tz0 U10 1tz0 17d0 1cL0 1cN0 1cL0 1cN0 1fz0 1cN0 1cL0 1cN0 1cL0 1cN0 1cL0 1cN0 1cL0 1cN0 1fz0 1cN0 1cL0 s10 1Vz0 1cN0 1cL0 1cN0 1cL0 6fd0 14n0|73e5');
var _elm_community$elm_time$Time_TimeZoneData$asia_hovd_l = _elm_community$elm_time$Time_TimeZoneData$unpack('Asia/Hovd|LMT HOVT HOVT HOVST|-66.A -60 -70 -80|012323232323232323232323232323232323232323232323232323232323232323232323232323232323232323232|-2APG6.A 2Uko6.A cKn0 1db0 1dd0 1cL0 1cN0 1cL0 1cN0 1cL0 1cN0 1cL0 1cN0 1cL0 1cN0 1cL0 1cN0 1fz0 1cN0 1cL0 1cN0 1cL0 1cN0 1cL0 1cN0 1cL0 1cN0 1cL0 1fB0 1cL0 1cN0 1cL0 1cN0 1cL0 6hD0 11z0 1cN0 1cL0 1cN0 1cL0 1cN0 1cL0 1cN0 1cL0 1cN0 1fz0 kEp0 1cJ0 1cP0 1cJ0 1cP0 1fx0 1cP0 1cJ0 1cP0 1cJ0 1cP0 1cJ0 1cP0 1cJ0 1cP0 1cJ0 1cP0 1fx0 1cP0 1cJ0 1cP0 1cJ0 1cP0 1cJ0 1cP0 1cJ0 1cP0 1fx0 1cP0 1cJ0 1cP0 1cJ0 1cP0 1cJ0 1cP0 1cJ0 1cP0 1cJ0 1cP0 1fx0 1cP0 1cJ0 1cP0 1cJ0 1cP0 1cJ0|81e3');
var _elm_community$elm_time$Time_TimeZoneData$asia_irkutsk_l = _elm_community$elm_time$Time_TimeZoneData$unpack('Asia/Irkutsk|IMT +07 +08 +09|-6V.5 -70 -80 -90|01232323232323232323232123232323232323232323232323232323232323232|-21zGV.5 pjXV.5 23CL0 1db0 1cN0 1db0 1cN0 1db0 1dd0 1cO0 1cM0 1cM0 1cM0 1cM0 1cM0 1cM0 1cM0 1cM0 1cM0 1cM0 1cM0 1fA0 2pB0 IM0 rX0 1cM0 1cM0 1cM0 1cM0 1cM0 1cM0 1cM0 1fA0 1o00 11A0 1o00 11A0 1o00 11A0 1qM0 WM0 1qM0 WM0 1qM0 11A0 1o00 11A0 1o00 11A0 1qM0 WM0 1qM0 WM0 1qM0 WM0 1qM0 11A0 1o00 11A0 1o00 11A0 1qM0 WM0 8Hz0|60e4');
var _elm_community$elm_time$Time_TimeZoneData$asia_jakarta_l = _elm_community$elm_time$Time_TimeZoneData$unpack('Asia/Jakarta|BMT JAVT WIB JST WIB WIB|-77.c -7k -7u -90 -80 -70|01232425|-1Q0Tk luM0 mPzO 8vWu 6kpu 4PXu xhcu|31e6');
var _elm_community$elm_time$Time_TimeZoneData$asia_jayapura_l = _elm_community$elm_time$Time_TimeZoneData$unpack('Asia/Jayapura|LMT WIT ACST|-9m.M -90 -9u|0121|-1uu9m.M sMMm.M L4nu|26e4');
var _elm_community$elm_time$Time_TimeZoneData$asia_jerusalem_l = _elm_community$elm_time$Time_TimeZoneData$unpack('Asia/Jerusalem|JMT IST IDT IDDT|-2k.E -20 -30 -40|01212121212132121212121212121212121212121212121212121212121212121212121212121212121212121212121212121212121212121212121212121212121212121212121|-26Bek.E SyMk.E 5Rb0 10r0 1px0 10N0 1pz0 16p0 1jB0 16p0 1jx0 3LB0 Em0 or0 1cn0 1dB0 16n0 10O0 1ja0 1tC0 14o0 1cM0 1a00 11A0 1Na0 An0 1MP0 AJ0 1Kp0 LC0 1oo0 Wl0 EQN0 Db0 1fB0 Rb0 npB0 11z0 1C10 IL0 1s10 10n0 1o10 WL0 1zd0 On0 1ld0 11z0 1o10 14n0 1o10 14n0 1nd0 12n0 1nd0 Xz0 1q10 12n0 1hB0 1dX0 1ep0 1aL0 1eN0 17X0 1nf0 11z0 1tB0 19W0 1e10 17b0 1ep0 1gL0 18N0 1fz0 1eN0 17b0 1gq0 1gn0 19d0 1dz0 1c10 17X0 1hB0 1gn0 19d0 1dz0 1c10 17X0 1kp0 1dz0 1c10 1aL0 1eN0 1oL0 10N0 1oL0 10N0 1oL0 10N0 1rz0 W10 1rz0 W10 1rz0 10N0 1oL0 10N0 1oL0 10N0 1rz0 W10 1rz0 W10 1rz0 10N0 1oL0 10N0 1oL0 10N0 1oL0 10N0 1rz0 W10 1rz0 W10 1rz0 10N0 1oL0 10N0 1oL0 10N0 1rz0 W10 1rz0 W10 1rz0 W10 1rz0 10N0 1oL0 10N0 1oL0|81e4');
var _elm_community$elm_time$Time_TimeZoneData$asia_kabul_l = _elm_community$elm_time$Time_TimeZoneData$unpack('Asia/Kabul|AFT AFT|-40 -4u|01|-10Qs0|46e5');
var _elm_community$elm_time$Time_TimeZoneData$asia_kamchatka_l = _elm_community$elm_time$Time_TimeZoneData$unpack('Asia/Kamchatka|LMT +11 +12 +13|-ay.A -b0 -c0 -d0|012323232323232323232321232323232323232323232323232323232323212|-1SLKy.A ivXy.A 23CL0 1db0 1cN0 1db0 1cN0 1db0 1dd0 1cO0 1cM0 1cM0 1cM0 1cM0 1cM0 1cM0 1cM0 1cM0 1cM0 1cM0 1cM0 1fA0 2pB0 IM0 rX0 1cM0 1cM0 1cM0 1cM0 1cM0 1cM0 1cM0 1fA0 1o00 11A0 1o00 11A0 1o00 11A0 1qM0 WM0 1qM0 WM0 1qM0 11A0 1o00 11A0 1o00 11A0 1qM0 WM0 1qM0 WM0 1qM0 WM0 1qM0 11A0 1o00 11A0 1o00 2sp0 WM0|18e4');
var _elm_community$elm_time$Time_TimeZoneData$asia_karachi_l = _elm_community$elm_time$Time_TimeZoneData$unpack('Asia/Karachi|LMT IST IST KART PKT PKST|-4s.c -5u -6u -50 -50 -60|012134545454|-2xoss.c 1qOKW.c 7zX0 eup0 LqMu 1fy00 1cL0 dK10 11b0 1610 1jX0|24e6');
var _elm_community$elm_time$Time_TimeZoneData$asia_kathmandu_l = _elm_community$elm_time$Time_TimeZoneData$unpack('Asia/Kathmandu|LMT IST NPT|-5F.g -5u -5J|012|-21JhF.g 2EGMb.g|12e5');
var _elm_community$elm_time$Time_TimeZoneData$asia_khandyga_l = _elm_community$elm_time$Time_TimeZoneData$unpack('Asia/Khandyga|LMT +08 +09 +10 +11|-92.d -80 -90 -a0 -b0|0123232323232323232323212323232323232323232323232343434343434343432|-21Q92.d pAp2.d 23CL0 1db0 1cN0 1db0 1cN0 1db0 1dd0 1cO0 1cM0 1cM0 1cM0 1cM0 1cM0 1cM0 1cM0 1cM0 1cM0 1cM0 1cM0 1fA0 2pB0 IM0 rX0 1cM0 1cM0 1cM0 1cM0 1cM0 1cM0 1cM0 1fA0 1o00 11A0 1o00 11A0 1o00 11A0 1qM0 WM0 1qM0 WM0 1qM0 11A0 1o00 11A0 1o00 qK0 yN0 1qM0 WM0 1qM0 WM0 1qM0 WM0 1qM0 11A0 1o00 11A0 1o00 11A0 1qM0 WM0 17V0 7zD0|66e2');
var _elm_community$elm_time$Time_TimeZoneData$asia_kolkata_l = _elm_community$elm_time$Time_TimeZoneData$unpack('Asia/Kolkata|HMT BURT IST IST|-5R.k -6u -5u -6u|01232|-18LFR.k 1unn.k HB0 7zX0|15e6');
var _elm_community$elm_time$Time_TimeZoneData$asia_krasnoyarsk_l = _elm_community$elm_time$Time_TimeZoneData$unpack('Asia/Krasnoyarsk|LMT +06 +07 +08|-6b.q -60 -70 -80|01232323232323232323232123232323232323232323232323232323232323232|-21Hib.q prAb.q 23CL0 1db0 1cN0 1db0 1cN0 1db0 1dd0 1cO0 1cM0 1cM0 1cM0 1cM0 1cM0 1cM0 1cM0 1cM0 1cM0 1cM0 1cM0 1fA0 2pB0 IM0 rX0 1cM0 1cM0 1cM0 1cM0 1cM0 1cM0 1cM0 1fA0 1o00 11A0 1o00 11A0 1o00 11A0 1qM0 WM0 1qM0 WM0 1qM0 11A0 1o00 11A0 1o00 11A0 1qM0 WM0 1qM0 WM0 1qM0 WM0 1qM0 11A0 1o00 11A0 1o00 11A0 1qM0 WM0 8Hz0|10e5');
var _elm_community$elm_time$Time_TimeZoneData$asia_kuala_lumpur_l = _elm_community$elm_time$Time_TimeZoneData$unpack('Asia/Kuala_Lumpur|SMT MALT MALST MALT MALT JST MYT|-6T.p -70 -7k -7k -7u -90 -80|01234546|-2Bg6T.p 17anT.p 7hXE dM00 17bO 8Fyu 1so1u|71e5');
var _elm_community$elm_time$Time_TimeZoneData$asia_kuching_l = _elm_community$elm_time$Time_TimeZoneData$unpack('Asia/Kuching|LMT BORT BORT BORTST JST MYT|-7l.k -7u -80 -8k -90 -80|01232323232323232425|-1KITl.k gDbP.k 6ynu AnE 1O0k AnE 1NAk AnE 1NAk AnE 1NAk AnE 1O0k AnE 1NAk AnE pAk 8Fz0 1so10|13e4');
var _elm_community$elm_time$Time_TimeZoneData$asia_macau_l = _elm_community$elm_time$Time_TimeZoneData$unpack('Asia/Macau|LMT MOT MOST CST|-7y.k -80 -90 -80|0121212121212121212121212121212121212121213|-2le7y.k 1XO34.k 1wn0 Rd0 1wn0 R9u 1wqu U10 1tz0 TVu 1tz0 17gu 1cL0 1cN0 1fz0 1cN0 1cL0 1cN0 1cL0 1cN0 1cL0 1cN0 1cL0 1cJu 1cL0 1cN0 1fz0 1cN0 1cOu 1cN0 1cL0 1cN0 1cL0 1cN0 1cL0 1cJu 1cL0 1cN0 1fz0 1cN0 1cL0 KEp0|57e4');
var _elm_community$elm_time$Time_TimeZoneData$asia_magadan_l = _elm_community$elm_time$Time_TimeZoneData$unpack('Asia/Magadan|LMT +10 +11 +12|-a3.c -a0 -b0 -c0|012323232323232323232321232323232323232323232323232323232323232312|-1Pca3.c eUo3.c 23CL0 1db0 1cN0 1db0 1cN0 1db0 1dd0 1cO0 1cM0 1cM0 1cM0 1cM0 1cM0 1cM0 1cM0 1cM0 1cM0 1cM0 1cM0 1fA0 2pB0 IM0 rX0 1cM0 1cM0 1cM0 1cM0 1cM0 1cM0 1cM0 1fA0 1o00 11A0 1o00 11A0 1o00 11A0 1qM0 WM0 1qM0 WM0 1qM0 11A0 1o00 11A0 1o00 11A0 1qM0 WM0 1qM0 WM0 1qM0 WM0 1qM0 11A0 1o00 11A0 1o00 11A0 1qM0 WM0 8Hz0 3Cq0|95e3');
var _elm_community$elm_time$Time_TimeZoneData$asia_makassar_l = _elm_community$elm_time$Time_TimeZoneData$unpack('Asia/Makassar|LMT MMT WITA JST|-7V.A -7V.A -80 -90|01232|-21JjV.A vfc0 myLV.A 8ML0|15e5');
var _elm_community$elm_time$Time_TimeZoneData$asia_manila_l = _elm_community$elm_time$Time_TimeZoneData$unpack('Asia/Manila|PHT PHST JST|-80 -90 -90|010201010|-1kJI0 AL0 cK10 65X0 mXB0 vX0 VK10 1db0|24e6');
var _elm_community$elm_time$Time_TimeZoneData$asia_nicosia_l = _elm_community$elm_time$Time_TimeZoneData$unpack('Asia/Nicosia|LMT EET EEST|-2d.s -20 -30|01212121212121212121212121212121212121212121212121212121212121212121212121212121212121212121212121212121212121212121212121212121|-1Vc2d.s 2a3cd.s 1cL0 1qp0 Xz0 19B0 19X0 1fB0 1db0 1cp0 1cL0 1fB0 19X0 1cN0 1cL0 1cN0 1cL0 1cN0 1cL0 1cN0 1fz0 1cN0 1cL0 1cN0 1cL0 1cN0 1cL0 1cN0 1cL0 1cN0 1cL0 1cN0 1fz0 1cN0 1cL0 1cN0 1cL0 1cN0 1cL0 1cN0 1cL0 1cN0 1cL0 1fB0 1cL0 1cN0 1cL0 1cN0 1o30 11A0 1qM0 WM0 1qM0 WM0 1qM0 11A0 1o00 11A0 1o00 11A0 1qM0 WM0 1qM0 WM0 1qM0 WM0 1qM0 11A0 1o00 11A0 1o00 11A0 1qM0 WM0 1qM0 WM0 1qM0 11A0 1o00 11A0 1o00 11A0 1o00 11A0 1qM0 WM0 1qM0 WM0 1qM0 11A0 1o00 11A0 1o00 11A0 1qM0 WM0 1qM0 WM0 1qM0 11A0 1o00 11A0 1o00 11A0 1o00 11A0 1qM0 WM0 1qM0 WM0 1qM0 11A0 1o00 11A0 1o00 11A0 1qM0 WM0 1qM0 WM0 1qM0 WM0 1qM0 11A0 1o00 11A0 1o00|32e4');
var _elm_community$elm_time$Time_TimeZoneData$asia_novokuznetsk_l = _elm_community$elm_time$Time_TimeZoneData$unpack('Asia/Novokuznetsk|LMT +06 +07 +08|-5M.M -60 -70 -80|012323232323232323232321232323232323232323232323232323232323212|-1PctM.M eULM.M 23CL0 1db0 1cN0 1db0 1cN0 1db0 1dd0 1cO0 1cM0 1cM0 1cM0 1cM0 1cM0 1cM0 1cM0 1cM0 1cM0 1cM0 1cM0 1fA0 2pB0 IM0 rX0 1cM0 1cM0 1cM0 1cM0 1cM0 1cM0 1cM0 1fA0 1o00 11A0 1o00 11A0 1o00 11A0 1qM0 WM0 1qM0 WM0 1qM0 11A0 1o00 11A0 1o00 11A0 1qM0 WM0 1qM0 WM0 1qM0 WM0 1qM0 11A0 1o00 11A0 1o00 2sp0 WM0|55e4');
var _elm_community$elm_time$Time_TimeZoneData$asia_novosibirsk_l = _elm_community$elm_time$Time_TimeZoneData$unpack('Asia/Novosibirsk|LMT +06 +07 +08|-5v.E -60 -70 -80|0123232323232323232323212323212121212121212121212121212121212121212|-21Qnv.E pAFv.E 23CL0 1db0 1cN0 1db0 1cN0 1db0 1dd0 1cO0 1cM0 1cM0 1cM0 1cM0 1cM0 1cM0 1cM0 1cM0 1cM0 1cM0 1cM0 1fA0 2pB0 IM0 rX0 1cM0 1cM0 ml0 Os0 1cM0 1cM0 1cM0 1cM0 1fA0 1o00 11A0 1o00 11A0 1o00 11A0 1qM0 WM0 1qM0 WM0 1qM0 11A0 1o00 11A0 1o00 11A0 1qM0 WM0 1qM0 WM0 1qM0 WM0 1qM0 11A0 1o00 11A0 1o00 11A0 1qM0 WM0 8Hz0 4eN0|15e5');
var _elm_community$elm_time$Time_TimeZoneData$asia_omsk_l = _elm_community$elm_time$Time_TimeZoneData$unpack('Asia/Omsk|LMT +05 +06 +07|-4R.u -50 -60 -70|01232323232323232323232123232323232323232323232323232323232323232|-224sR.u pMLR.u 23CL0 1db0 1cN0 1db0 1cN0 1db0 1dd0 1cO0 1cM0 1cM0 1cM0 1cM0 1cM0 1cM0 1cM0 1cM0 1cM0 1cM0 1cM0 1fA0 2pB0 IM0 rX0 1cM0 1cM0 1cM0 1cM0 1cM0 1cM0 1cM0 1fA0 1o00 11A0 1o00 11A0 1o00 11A0 1qM0 WM0 1qM0 WM0 1qM0 11A0 1o00 11A0 1o00 11A0 1qM0 WM0 1qM0 WM0 1qM0 WM0 1qM0 11A0 1o00 11A0 1o00 11A0 1qM0 WM0 8Hz0|12e5');
var _elm_community$elm_time$Time_TimeZoneData$asia_oral_l = _elm_community$elm_time$Time_TimeZoneData$unpack('Asia/Oral|LMT +04 +05 +06|-3p.o -40 -50 -60|01232323232323232121212121212121212121212121212|-1Pc3p.o eUnp.o 23CL0 3Db0 1cN0 1db0 1dd0 1cO0 1cM0 1cM0 1cM0 1cM0 1cM0 1cM0 1cM0 1cM0 2pB0 1cM0 1fA0 1cM0 1cM0 IM0 1EM0 1cM0 1cM0 1cM0 1cM0 1cM0 1cM0 1fA0 1o00 11A0 1o00 11A0 1o00 11A0 1qM0 WM0 1qM0 WM0 1qM0 11A0 1o00 11A0 1o00 11A0|27e4');
var _elm_community$elm_time$Time_TimeZoneData$asia_pontianak_l = _elm_community$elm_time$Time_TimeZoneData$unpack('Asia/Pontianak|LMT PMT WIB JST WIB WITA WIB|-7h.k -7h.k -7u -90 -80 -80 -70|012324256|-2ua7h.k XE00 munL.k 8Rau 6kpu 4PXu xhcu Wqnu|23e4');
var _elm_community$elm_time$Time_TimeZoneData$asia_pyongyang_l = _elm_community$elm_time$Time_TimeZoneData$unpack('Asia/Pyongyang|LMT KST JCST JST KST|-8n -8u -90 -90 -90|012341|-2um8n 97XR 12FXu jdA0 2Onc0|29e5');
var _elm_community$elm_time$Time_TimeZoneData$asia_qatar_l = _elm_community$elm_time$Time_TimeZoneData$unpack('Asia/Qatar|LMT GST AST|-3q.8 -40 -30|012|-21Jfq.8 27BXq.8|96e4');
var _elm_community$elm_time$Time_TimeZoneData$asia_qyzylorda_l = _elm_community$elm_time$Time_TimeZoneData$unpack('Asia/Qyzylorda|LMT +04 +05 +06|-4l.Q -40 -50 -60|0123232323232323232323232323232323232323232323|-1Pc4l.Q eUol.Q 23CL0 3Db0 1cN0 1db0 1dd0 1cO0 1cM0 1cM0 1cM0 1cM0 1cM0 1cM0 1cM0 1cM0 1cM0 1cM0 1cM0 1fA0 3ao0 1EM0 1cM0 1cM0 1cM0 1cM0 1cM0 1cM0 1fA0 1o00 11A0 1o00 11A0 1o00 11A0 1qM0 WM0 1qM0 WM0 1qM0 11A0 1o00 11A0 1o00 11A0|73e4');
var _elm_community$elm_time$Time_TimeZoneData$asia_rangoon_l = _elm_community$elm_time$Time_TimeZoneData$unpack('Asia/Rangoon|RMT BURT JST MMT|-6o.E -6u -90 -6u|0123|-21Jio.E SmnS.E 7j9u|48e5');
var _elm_community$elm_time$Time_TimeZoneData$asia_riyadh_l = _elm_community$elm_time$Time_TimeZoneData$unpack('Asia/Riyadh|LMT AST|-36.Q -30|01|-TvD6.Q|57e5');
var _elm_community$elm_time$Time_TimeZoneData$asia_sakhalin_l = _elm_community$elm_time$Time_TimeZoneData$unpack('Asia/Sakhalin|LMT +09 +11 +12 +10|-9u.M -90 -b0 -c0 -a0|01232323232323232323232423232323232424242424242424242424242424242|-2AGVu.M 1BoMu.M 1qFa0 1db0 1cN0 1db0 1cN0 1db0 1dd0 1cO0 1cM0 1cM0 1cM0 1cM0 1cM0 1cM0 1cM0 1cM0 1cM0 1cM0 1cM0 1fA0 2pB0 IM0 rX0 1cM0 1cM0 1cM0 1cM0 1cM0 1cM0 1cM0 1fA0 1o00 2pB0 11A0 1o00 11A0 1qM0 WM0 1qM0 WM0 1qM0 11A0 1o00 11A0 1o00 11A0 1qM0 WM0 1qM0 WM0 1qM0 WM0 1qM0 11A0 1o00 11A0 1o00 11A0 1qM0 WM0 8Hz0 3rd0|58e4');
var _elm_community$elm_time$Time_TimeZoneData$asia_samarkand_l = _elm_community$elm_time$Time_TimeZoneData$unpack('Asia/Samarkand|LMT +04 +05 +06|-4r.R -40 -50 -60|01232323232323232323232|-1Pc4r.R eUor.R 23CL0 3Db0 1cN0 1db0 1dd0 1cO0 1cM0 1cM0 1cM0 1cM0 1cM0 1cM0 1cM0 1cM0 1cM0 1cM0 1cM0 1fA0 1cM0 1cM0|36e4');
var _elm_community$elm_time$Time_TimeZoneData$asia_seoul_l = _elm_community$elm_time$Time_TimeZoneData$unpack('Asia/Seoul|LMT KST JCST JST KST KDT KDT|-8r.Q -8u -90 -90 -90 -9u -a0|01234151515151515146464|-2um8r.Q 97XV.Q 12FXu jjA0 kKo0 2I0u OL0 1FB0 Rb0 1qN0 TX0 1tB0 TX0 1tB0 TX0 1tB0 TX0 2ap0 12FBu 11A0 1o00 11A0|23e6');
var _elm_community$elm_time$Time_TimeZoneData$asia_shanghai_l = _elm_community$elm_time$Time_TimeZoneData$unpack('Asia/Shanghai|CST CDT|-80 -90|01010101010101010|-1c1I0 LX0 16p0 1jz0 1Myp0 Rb0 1o10 11z0 1o10 11z0 1qN0 11z0 1o10 11z0 1o10 11z0|23e6');
var _elm_community$elm_time$Time_TimeZoneData$asia_singapore_l = _elm_community$elm_time$Time_TimeZoneData$unpack('Asia/Singapore|SMT MALT MALST MALT MALT JST SGT SGT|-6T.p -70 -7k -7k -7u -90 -7u -80|012345467|-2Bg6T.p 17anT.p 7hXE dM00 17bO 8Fyu Mspu DTA0|56e5');
var _elm_community$elm_time$Time_TimeZoneData$asia_srednekolymsk_l = _elm_community$elm_time$Time_TimeZoneData$unpack('Asia/Srednekolymsk|LMT +10 +11 +12|-ae.Q -a0 -b0 -c0|01232323232323232323232123232323232323232323232323232323232323232|-1Pcae.Q eUoe.Q 23CL0 1db0 1cN0 1db0 1cN0 1db0 1dd0 1cO0 1cM0 1cM0 1cM0 1cM0 1cM0 1cM0 1cM0 1cM0 1cM0 1cM0 1cM0 1fA0 2pB0 IM0 rX0 1cM0 1cM0 1cM0 1cM0 1cM0 1cM0 1cM0 1fA0 1o00 11A0 1o00 11A0 1o00 11A0 1qM0 WM0 1qM0 WM0 1qM0 11A0 1o00 11A0 1o00 11A0 1qM0 WM0 1qM0 WM0 1qM0 WM0 1qM0 11A0 1o00 11A0 1o00 11A0 1qM0 WM0 8Hz0|35e2');
var _elm_community$elm_time$Time_TimeZoneData$asia_taipei_l = _elm_community$elm_time$Time_TimeZoneData$unpack('Asia/Taipei|JWST JST CST CDT|-80 -90 -80 -90|01232323232323232323232323232323232323232|-1iw80 joM0 1yo0 Tz0 1ip0 1jX0 1cN0 11b0 1oN0 11b0 1oN0 11b0 1oN0 11b0 10N0 1BX0 10p0 1pz0 10p0 1pz0 10p0 1db0 1dd0 1db0 1cN0 1db0 1cN0 1db0 1cN0 1db0 1BB0 ML0 1Bd0 ML0 uq10 1db0 1cN0 1db0 97B0 AL0|74e5');
var _elm_community$elm_time$Time_TimeZoneData$asia_tashkent_l = _elm_community$elm_time$Time_TimeZoneData$unpack('Asia/Tashkent|LMT +05 +06 +07|-4B.b -50 -60 -70|012323232323232323232321|-1Pc4B.b eUnB.b 23CL0 1db0 1cN0 1db0 1cN0 1db0 1dd0 1cO0 1cM0 1cM0 1cM0 1cM0 1cM0 1cM0 1cM0 1cM0 1cM0 1cM0 1cM0 1fA0 2pB0|23e5');
var _elm_community$elm_time$Time_TimeZoneData$asia_tbilisi_l = _elm_community$elm_time$Time_TimeZoneData$unpack('Asia/Tbilisi|TBMT +03 +04 +05|-2X.b -30 -40 -50|0123232323232323232323212121232323232323232323212|-1Pc2X.b 1jUnX.b WCL0 1db0 1cN0 1db0 1cN0 1db0 1dd0 1cO0 1cM0 1cM0 1cM0 1cM0 1cM0 1cM0 1cM0 1cM0 1cM0 1cM0 1cM0 1fA0 2pB0 1cK0 1cL0 1cN0 1cL0 1cN0 2pz0 1cL0 1fB0 3Nz0 11B0 1nX0 11B0 1qL0 WN0 1qL0 WN0 1qL0 11B0 1nX0 11B0 1nX0 11B0 An0 Os0 WM0|11e5');
var _elm_community$elm_time$Time_TimeZoneData$asia_tehran_l = _elm_community$elm_time$Time_TimeZoneData$unpack('Asia/Tehran|LMT TMT IRST IRST IRDT IRDT|-3p.I -3p.I -3u -40 -50 -4u|01234325252525252525252525252525252525252525252525252525252525252525252525252525252525252525252525252|-2btDp.I 1d3c0 1huLT.I TXu 1pz0 sN0 vAu 1cL0 1dB0 1en0 pNB0 UL0 1cN0 1dz0 1cp0 1dz0 1cp0 1dz0 1cp0 1dz0 1cp0 1dz0 1cN0 1dz0 1cp0 1dz0 1cp0 1dz0 1cp0 1dz0 1cN0 1dz0 1cp0 1dz0 1cp0 1dz0 1cp0 1dz0 1cN0 1dz0 64p0 1dz0 1cN0 1dz0 1cp0 1dz0 1cp0 1dz0 1cp0 1dz0 1cN0 1dz0 1cp0 1dz0 1cp0 1dz0 1cp0 1dz0 1cN0 1dz0 1cp0 1dz0 1cp0 1dz0 1cp0 1dz0 1cN0 1dz0 1cp0 1dz0 1cp0 1dz0 1cp0 1dz0 1cN0 1dz0 1cp0 1dz0 1cp0 1dz0 1cp0 1dz0 1cp0 1dz0 1cN0 1dz0 1cp0 1dz0 1cp0 1dz0 1cp0 1dz0 1cN0 1dz0 1cp0 1dz0 1cp0 1dz0 1cp0 1dz0|14e6');
var _elm_community$elm_time$Time_TimeZoneData$asia_thimphu_l = _elm_community$elm_time$Time_TimeZoneData$unpack('Asia/Thimphu|LMT IST BTT|-5W.A -5u -60|012|-Su5W.A 1BGMs.A|79e3');
var _elm_community$elm_time$Time_TimeZoneData$asia_tokyo_l = _elm_community$elm_time$Time_TimeZoneData$unpack('Asia/Tokyo|JCST JST JDT|-90 -90 -a0|0121212121|-1iw90 pKq0 QL0 1lB0 13X0 1zB0 NX0 1zB0 NX0|38e6');
var _elm_community$elm_time$Time_TimeZoneData$asia_tomsk_l = _elm_community$elm_time$Time_TimeZoneData$unpack('Asia/Tomsk|LMT +06 +07 +08|-5D.P -60 -70 -80|0123232323232323232323212323232323232323232323212121212121212121212|-21NhD.P pxzD.P 23CL0 1db0 1cN0 1db0 1cN0 1db0 1dd0 1cO0 1cM0 1cM0 1cM0 1cM0 1cM0 1cM0 1cM0 1cM0 1cM0 1cM0 1cM0 1fA0 2pB0 IM0 rX0 1cM0 1cM0 1cM0 1cM0 1cM0 1cM0 1cM0 1fA0 1o00 11A0 1o00 11A0 1o00 11A0 1qM0 WM0 1qM0 WM0 1qM0 11A0 co0 1bB0 11A0 1o00 11A0 1qM0 WM0 1qM0 WM0 1qM0 WM0 1qM0 11A0 1o00 11A0 1o00 11A0 1qM0 WM0 8Hz0 3Qp0|10e5');
var _elm_community$elm_time$Time_TimeZoneData$asia_ulaanbaatar_l = _elm_community$elm_time$Time_TimeZoneData$unpack('Asia/Ulaanbaatar|LMT ULAT ULAT ULAST|-77.w -70 -80 -90|012323232323232323232323232323232323232323232323232323232323232323232323232323232323232323232|-2APH7.w 2Uko7.w cKn0 1db0 1dd0 1cL0 1cN0 1cL0 1cN0 1cL0 1cN0 1cL0 1cN0 1cL0 1cN0 1cL0 1cN0 1fz0 1cN0 1cL0 1cN0 1cL0 1cN0 1cL0 1cN0 1cL0 1cN0 1cL0 1fB0 1cL0 1cN0 1cL0 1cN0 1cL0 6hD0 11z0 1cN0 1cL0 1cN0 1cL0 1cN0 1cL0 1cN0 1cL0 1cN0 1fz0 kEp0 1cJ0 1cP0 1cJ0 1cP0 1fx0 1cP0 1cJ0 1cP0 1cJ0 1cP0 1cJ0 1cP0 1cJ0 1cP0 1cJ0 1cP0 1fx0 1cP0 1cJ0 1cP0 1cJ0 1cP0 1cJ0 1cP0 1cJ0 1cP0 1fx0 1cP0 1cJ0 1cP0 1cJ0 1cP0 1cJ0 1cP0 1cJ0 1cP0 1cJ0 1cP0 1fx0 1cP0 1cJ0 1cP0 1cJ0 1cP0 1cJ0|12e5');
var _elm_community$elm_time$Time_TimeZoneData$asia_urumqi_l = _elm_community$elm_time$Time_TimeZoneData$unpack('Asia/Urumqi|LMT XJT|-5O.k -60|01|-1GgtO.k|32e5');
var _elm_community$elm_time$Time_TimeZoneData$asia_ust_nera_l = _elm_community$elm_time$Time_TimeZoneData$unpack('Asia/Ust-Nera|LMT +08 +09 +12 +11 +10|-9w.S -80 -90 -c0 -b0 -a0|012343434343434343434345434343434343434343434343434343434343434345|-21Q9w.S pApw.S 23CL0 1d90 1cN0 1db0 1cN0 1db0 1dd0 1cO0 1cM0 1cM0 1cM0 1cM0 1cM0 1cM0 1cM0 1cM0 1cM0 1cM0 1cM0 1fA0 2pB0 IM0 rX0 1cM0 1cM0 1cM0 1cM0 1cM0 1cM0 1cM0 1fA0 1o00 11A0 1o00 11A0 1o00 11A0 1qM0 WM0 1qM0 WM0 1qM0 11A0 1o00 11A0 1o00 11A0 1qM0 WM0 1qM0 WM0 1qM0 WM0 1qM0 11A0 1o00 11A0 1o00 11A0 1qM0 WM0 17V0 7zD0|65e2');
var _elm_community$elm_time$Time_TimeZoneData$asia_vladivostok_l = _elm_community$elm_time$Time_TimeZoneData$unpack('Asia/Vladivostok|LMT +09 +10 +11|-8L.v -90 -a0 -b0|01232323232323232323232123232323232323232323232323232323232323232|-1SJIL.v itXL.v 23CL0 1db0 1cN0 1db0 1cN0 1db0 1dd0 1cO0 1cM0 1cM0 1cM0 1cM0 1cM0 1cM0 1cM0 1cM0 1cM0 1cM0 1cM0 1fA0 2pB0 IM0 rX0 1cM0 1cM0 1cM0 1cM0 1cM0 1cM0 1cM0 1fA0 1o00 11A0 1o00 11A0 1o00 11A0 1qM0 WM0 1qM0 WM0 1qM0 11A0 1o00 11A0 1o00 11A0 1qM0 WM0 1qM0 WM0 1qM0 WM0 1qM0 11A0 1o00 11A0 1o00 11A0 1qM0 WM0 8Hz0|60e4');
var _elm_community$elm_time$Time_TimeZoneData$asia_yakutsk_l = _elm_community$elm_time$Time_TimeZoneData$unpack('Asia/Yakutsk|LMT +08 +09 +10|-8C.W -80 -90 -a0|01232323232323232323232123232323232323232323232323232323232323232|-21Q8C.W pAoC.W 23CL0 1db0 1cN0 1db0 1cN0 1db0 1dd0 1cO0 1cM0 1cM0 1cM0 1cM0 1cM0 1cM0 1cM0 1cM0 1cM0 1cM0 1cM0 1fA0 2pB0 IM0 rX0 1cM0 1cM0 1cM0 1cM0 1cM0 1cM0 1cM0 1fA0 1o00 11A0 1o00 11A0 1o00 11A0 1qM0 WM0 1qM0 WM0 1qM0 11A0 1o00 11A0 1o00 11A0 1qM0 WM0 1qM0 WM0 1qM0 WM0 1qM0 11A0 1o00 11A0 1o00 11A0 1qM0 WM0 8Hz0|28e4');
var _elm_community$elm_time$Time_TimeZoneData$asia_yekaterinburg_l = _elm_community$elm_time$Time_TimeZoneData$unpack('Asia/Yekaterinburg|LMT PMT +04 +05 +06|-42.x -3J.5 -40 -50 -60|012343434343434343434343234343434343434343434343434343434343434343|-2ag42.x 7mQh.s qBvJ.5 23CL0 1db0 1cN0 1db0 1cN0 1db0 1dd0 1cO0 1cM0 1cM0 1cM0 1cM0 1cM0 1cM0 1cM0 1cM0 1cM0 1cM0 1cM0 1fA0 2pB0 IM0 rX0 1cM0 1cM0 1cM0 1cM0 1cM0 1cM0 1cM0 1fA0 1o00 11A0 1o00 11A0 1o00 11A0 1qM0 WM0 1qM0 WM0 1qM0 11A0 1o00 11A0 1o00 11A0 1qM0 WM0 1qM0 WM0 1qM0 WM0 1qM0 11A0 1o00 11A0 1o00 11A0 1qM0 WM0 8Hz0|14e5');
var _elm_community$elm_time$Time_TimeZoneData$asia_yerevan_l = _elm_community$elm_time$Time_TimeZoneData$unpack('Asia/Yerevan|LMT +03 +04 +05|-2W -30 -40 -50|0123232323232323232323212121212323232323232323232323232323232|-1Pc2W 1jUnW WCL0 1db0 1cN0 1db0 1cN0 1db0 1dd0 1cO0 1cM0 1cM0 1cM0 1cM0 1cM0 1cM0 1cM0 1cM0 1cM0 1cM0 1cM0 1fA0 2pB0 1cM0 1cM0 1cM0 1cM0 1cM0 1cM0 1cM0 4RX0 1o00 11A0 1o00 11A0 1qM0 WM0 1qM0 WM0 1qM0 11A0 1o00 11A0 1o00 11A0 1qM0 WM0 1qM0 WM0 1qM0 WM0 1qM0 11A0 1o00 11A0 1o00 11A0 1qM0 WM0 1qM0|13e5');
var _elm_community$elm_time$Time_TimeZoneData$atlantic_azores_l = _elm_community$elm_time$Time_TimeZoneData$unpack('Atlantic/Azores|HMT AZOT AZOST AZOMT AZOT AZOST WET|1S.w 20 10 0 10 0 0|01212121212121212121212121212121212121212121232123212321232121212121212121212121212121212121212121454545454545454545454545454545456545454545454545454545454545454545454545454545454545454545454545454545454545454545454545454|-2ldW5.s aPX5.s Sp0 LX0 1vc0 Tc0 1uM0 SM0 1vc0 Tc0 1vc0 SM0 1vc0 6600 1co0 3E00 17c0 1fA0 1a00 1io0 1a00 1io0 17c0 3I00 17c0 1cM0 1cM0 3Fc0 1cM0 1a00 1fA0 1io0 17c0 1cM0 1cM0 1a00 1fA0 1io0 1qM0 Dc0 1tA0 1cM0 1dc0 1400 gL0 IM0 s10 U00 dX0 Rc0 pd0 Rc0 gL0 Oo0 pd0 Rc0 gL0 Oo0 pd0 14o0 1cM0 1cP0 1cM0 1cM0 1cM0 1cM0 1cM0 3Co0 1fA0 1cM0 1cM0 1cM0 1cM0 1cM0 1cM0 1cM0 1cM0 1cM0 1fA0 1cM0 1cM0 1cM0 1cM0 1cM0 1cM0 1cM0 1cM0 1cM0 1cM0 1cM0 1fA0 1cM0 1cM0 1cM0 1cM0 1cM0 1cM0 1cM0 qIl0 1cM0 1fA0 1cM0 1cM0 1cN0 1cL0 1cN0 1cM0 1cM0 1cM0 1cM0 1cN0 1cL0 1cM0 1fA0 1cM0 1cM0 1cM0 1cM0 1cM0 1cM0 1cM0 1cM0 1cM0 1cM0 1cM0 1fA0 1cM0 1cM0 1cM0 1cM0 1cL0 1cM0 1cM0 1cM0 1cM0 1cM0 1fA0 1o00 11A0 1o00 11A0 1o00 11A0 1qM0 WM0 1qM0 WM0 1qM0 11A0 1o00 11A0 1o00 11A0 1qM0 WM0 1qM0 WM0 1qM0 WM0 1qM0 11A0 1o00 11A0 1o00 11A0 1qM0 WM0 1qM0 WM0 1qM0 11A0 1o00 11A0 1o00 11A0 1o00 11A0 1qM0 WM0 1qM0 WM0 1qM0 11A0 1o00 11A0 1o00 11A0 1qM0 WM0 1qM0 WM0 1qM0 11A0 1o00 11A0 1o00 11A0 1o00 11A0 1qM0 WM0 1qM0 WM0 1qM0 11A0 1o00 11A0 1o00 11A0 1qM0 WM0 1qM0 WM0 1qM0 WM0 1qM0 11A0 1o00 11A0 1o00|25e4');
var _elm_community$elm_time$Time_TimeZoneData$atlantic_bermuda_l = _elm_community$elm_time$Time_TimeZoneData$unpack('Atlantic/Bermuda|LMT AST ADT|4j.i 40 30|0121212121212121212121212121212121212121212121212121212121212121212121212121212121212121212121212121212121212121212121212121212121|-1BnRE.G 1LTbE.G 1cL0 1cN0 1cL0 1cN0 1fz0 1a10 1fz0 1cN0 1cL0 1cN0 1cL0 1cN0 1cL0 1cN0 1cL0 1cN0 1fz0 1a10 1fz0 1cN0 1cL0 1cN0 1cL0 1cN0 1cL0 14p0 1lb0 14p0 1nX0 11B0 1nX0 11B0 1nX0 14p0 1lb0 14p0 1lb0 14p0 1nX0 11B0 1nX0 11B0 1nX0 14p0 1lb0 14p0 1lb0 14p0 1lb0 14p0 1nX0 11B0 1nX0 11B0 1nX0 14p0 1lb0 14p0 1lb0 14p0 1nX0 11B0 1nX0 11B0 1nX0 Rd0 1zb0 Op0 1zb0 Op0 1zb0 Rd0 1zb0 Op0 1zb0 Op0 1zb0 Op0 1zb0 Op0 1zb0 Op0 1zb0 Rd0 1zb0 Op0 1zb0 Op0 1zb0 Op0 1zb0 Op0 1zb0 Rd0 1zb0 Op0 1zb0 Op0 1zb0 Op0 1zb0 Op0 1zb0 Op0 1zb0 Rd0 1zb0 Op0 1zb0 Op0 1zb0 Op0 1zb0 Op0 1zb0 Rd0 1zb0 Op0 1zb0 Op0 1zb0 Op0 1zb0 Op0 1zb0 Op0 1zb0|65e3');
var _elm_community$elm_time$Time_TimeZoneData$atlantic_canary_l = _elm_community$elm_time$Time_TimeZoneData$unpack('Atlantic/Canary|LMT CANT WET WEST|11.A 10 0 -10|01232323232323232323232323232323232323232323232323232323232323232323232323232323232323232323232323232323232323232323232|-1UtaW.o XPAW.o 1lAK0 1a10 1cM0 1cM0 1cM0 1cM0 1cM0 1cM0 1cM0 1fA0 1cM0 1cM0 1cM0 1cM0 1cM0 1cM0 1cM0 1cM0 1cM0 1cM0 1cM0 1fA0 1cM0 1cM0 1cM0 1cM0 1cM0 1cM0 1cM0 1cM0 1cM0 1cM0 1fA0 1o00 11A0 1o00 11A0 1o00 11A0 1qM0 WM0 1qM0 WM0 1qM0 11A0 1o00 11A0 1o00 11A0 1qM0 WM0 1qM0 WM0 1qM0 WM0 1qM0 11A0 1o00 11A0 1o00 11A0 1qM0 WM0 1qM0 WM0 1qM0 11A0 1o00 11A0 1o00 11A0 1o00 11A0 1qM0 WM0 1qM0 WM0 1qM0 11A0 1o00 11A0 1o00 11A0 1qM0 WM0 1qM0 WM0 1qM0 11A0 1o00 11A0 1o00 11A0 1o00 11A0 1qM0 WM0 1qM0 WM0 1qM0 11A0 1o00 11A0 1o00 11A0 1qM0 WM0 1qM0 WM0 1qM0 WM0 1qM0 11A0 1o00 11A0 1o00|54e4');
var _elm_community$elm_time$Time_TimeZoneData$atlantic_cape_verde_l = _elm_community$elm_time$Time_TimeZoneData$unpack('Atlantic/Cape_Verde|LMT CVT CVST CVT|1y.4 20 10 10|01213|-2xomp.U 1qOMp.U 7zX0 1djf0|50e4');
var _elm_community$elm_time$Time_TimeZoneData$atlantic_faroe_l = _elm_community$elm_time$Time_TimeZoneData$unpack('Atlantic/Faroe|LMT WET WEST|r.4 0 -10|01212121212121212121212121212121212121212121212121212121212121212121212121212121212121212121212121212121212121212121|-2uSnw.U 2Wgow.U 1cM0 1cM0 1cM0 1cM0 1cM0 1cM0 1fA0 1cM0 1cM0 1cM0 1cM0 1cM0 1cM0 1cM0 1cM0 1cM0 1cM0 1cM0 1fA0 1cM0 1cM0 1cM0 1cM0 1cM0 1cM0 1cM0 1cM0 1cM0 1cM0 1fA0 1o00 11A0 1o00 11A0 1o00 11A0 1qM0 WM0 1qM0 WM0 1qM0 11A0 1o00 11A0 1o00 11A0 1qM0 WM0 1qM0 WM0 1qM0 WM0 1qM0 11A0 1o00 11A0 1o00 11A0 1qM0 WM0 1qM0 WM0 1qM0 11A0 1o00 11A0 1o00 11A0 1o00 11A0 1qM0 WM0 1qM0 WM0 1qM0 11A0 1o00 11A0 1o00 11A0 1qM0 WM0 1qM0 WM0 1qM0 11A0 1o00 11A0 1o00 11A0 1o00 11A0 1qM0 WM0 1qM0 WM0 1qM0 11A0 1o00 11A0 1o00 11A0 1qM0 WM0 1qM0 WM0 1qM0 WM0 1qM0 11A0 1o00 11A0 1o00|49e3');
var _elm_community$elm_time$Time_TimeZoneData$atlantic_madeira_l = _elm_community$elm_time$Time_TimeZoneData$unpack('Atlantic/Madeira|FMT MADT MADST MADMT WET WEST|17.A 10 0 -10 0 -10|01212121212121212121212121212121212121212121232123212321232121212121212121212121212121212121212121454545454545454545454545454545454545454545454545454545454545454545454545454545454545454545454545454545454545454545454545454|-2ldWQ.o aPWQ.o Sp0 LX0 1vc0 Tc0 1uM0 SM0 1vc0 Tc0 1vc0 SM0 1vc0 6600 1co0 3E00 17c0 1fA0 1a00 1io0 1a00 1io0 17c0 3I00 17c0 1cM0 1cM0 3Fc0 1cM0 1a00 1fA0 1io0 17c0 1cM0 1cM0 1a00 1fA0 1io0 1qM0 Dc0 1tA0 1cM0 1dc0 1400 gL0 IM0 s10 U00 dX0 Rc0 pd0 Rc0 gL0 Oo0 pd0 Rc0 gL0 Oo0 pd0 14o0 1cM0 1cP0 1cM0 1cM0 1cM0 1cM0 1cM0 3Co0 1fA0 1cM0 1cM0 1cM0 1cM0 1cM0 1cM0 1cM0 1cM0 1cM0 1fA0 1cM0 1cM0 1cM0 1cM0 1cM0 1cM0 1cM0 1cM0 1cM0 1cM0 1cM0 1fA0 1cM0 1cM0 1cM0 1cM0 1cM0 1cM0 1cM0 qIl0 1cM0 1fA0 1cM0 1cM0 1cN0 1cL0 1cN0 1cM0 1cM0 1cM0 1cM0 1cN0 1cL0 1cM0 1fA0 1cM0 1cM0 1cM0 1cM0 1cM0 1cM0 1cM0 1cM0 1cM0 1cM0 1cM0 1fA0 1cM0 1cM0 1cM0 1cM0 1cM0 1cM0 1cM0 1cM0 1cM0 1cM0 1fA0 1o00 11A0 1o00 11A0 1o00 11A0 1qM0 WM0 1qM0 WM0 1qM0 11A0 1o00 11A0 1o00 11A0 1qM0 WM0 1qM0 WM0 1qM0 WM0 1qM0 11A0 1o00 11A0 1o00 11A0 1qM0 WM0 1qM0 WM0 1qM0 11A0 1o00 11A0 1o00 11A0 1o00 11A0 1qM0 WM0 1qM0 WM0 1qM0 11A0 1o00 11A0 1o00 11A0 1qM0 WM0 1qM0 WM0 1qM0 11A0 1o00 11A0 1o00 11A0 1o00 11A0 1qM0 WM0 1qM0 WM0 1qM0 11A0 1o00 11A0 1o00 11A0 1qM0 WM0 1qM0 WM0 1qM0 WM0 1qM0 11A0 1o00 11A0 1o00|27e4');
var _elm_community$elm_time$Time_TimeZoneData$atlantic_reykjavik_l = _elm_community$elm_time$Time_TimeZoneData$unpack('Atlantic/Reykjavik|LMT IST ISST GMT|1s 10 0 0|012121212121212121212121212121212121212121212121212121212121212121213|-2uWmw mfaw 1Bd0 ML0 1LB0 Cn0 1LB0 3fX0 C10 HrX0 1cO0 LB0 1EL0 LA0 1C00 Oo0 1wo0 Rc0 1wo0 Rc0 1wo0 Rc0 1zc0 Oo0 1zc0 14o0 1lc0 14o0 1lc0 14o0 1o00 11A0 1lc0 14o0 1o00 14o0 1lc0 14o0 1lc0 14o0 1lc0 14o0 1lc0 14o0 1o00 14o0 1lc0 14o0 1lc0 14o0 1lc0 14o0 1lc0 14o0 1lc0 14o0 1o00 14o0 1lc0 14o0 1lc0 14o0 1lc0 14o0 1lc0 14o0 1o00 14o0|12e4');
var _elm_community$elm_time$Time_TimeZoneData$atlantic_south_georgia_l = _elm_community$elm_time$Time_TimeZoneData$unpack('Atlantic/South_Georgia|GST|20|0||30');
var _elm_community$elm_time$Time_TimeZoneData$atlantic_stanley_l = _elm_community$elm_time$Time_TimeZoneData$unpack('Atlantic/Stanley|SMT FKT FKST FKT FKST|3P.o 40 30 30 20|0121212121212134343212121212121212121212121212121212121212121212121212|-2kJw8.A 12bA8.A 19X0 1fB0 19X0 1ip0 19X0 1fB0 19X0 1fB0 19X0 1fB0 Cn0 1Cc10 WL0 1qL0 U10 1tz0 U10 1qM0 WN0 1qL0 WN0 1qL0 WN0 1qL0 WN0 1tz0 U10 1tz0 WN0 1qL0 WN0 1qL0 WN0 1qL0 WN0 1qL0 WN0 1tz0 WN0 1qL0 WN0 1qL0 WN0 1qL0 WN0 1qL0 WN0 1qN0 U10 1wn0 Rd0 1wn0 U10 1tz0 U10 1tz0 U10 1tz0 U10 1tz0 U10 1wn0 U10 1tz0 U10 1tz0 U10|21e2');
var _elm_community$elm_time$Time_TimeZoneData$australia_adelaide_l = _elm_community$elm_time$Time_TimeZoneData$unpack('Australia/Adelaide|ACST ACDT|-9u -au|0101010101010101010101010101010101010101010101010101010101010101010101010101010101010101010101010101010101010101010101010101010101010101010101|-293lt xcX 10jd0 yL0 1cN0 1cL0 1fB0 19X0 17c10 LA0 1C00 Oo0 1zc0 Oo0 1zc0 Oo0 1zc0 Rc0 1zc0 Oo0 1zc0 Oo0 1zc0 Oo0 1zc0 Oo0 1zc0 Oo0 1zc0 Rc0 1zc0 Oo0 1zc0 Oo0 1zc0 Oo0 1zc0 U00 1qM0 WM0 1tA0 WM0 1tA0 U00 1tA0 U00 1tA0 Oo0 1zc0 WM0 1qM0 Rc0 1zc0 U00 1tA0 WM0 1qM0 11A0 1o00 11A0 1o00 11A0 1o00 11A0 1qM0 WM0 1qM0 WM0 1qM0 11A0 1o00 11A0 1o00 11A0 1qM0 WM0 1qM0 11A0 1o00 WM0 1qM0 14o0 1cM0 1cM0 1cM0 1cM0 1cM0 1cM0 1cM0 1cM0 1fA0 1cM0 1cM0 1cM0 1cM0 1cM0 1cM0 1cM0 1cM0 1cM0 1cM0 1cM0 1fA0 1cM0 1cM0 1cM0 1cM0 1cM0 1cM0 1cM0 1cM0 1cM0 1cM0 1fA0 1cM0 1cM0 1cM0 1cM0 1cM0 1cM0 1cM0 1cM0 1cM0 1cM0 1fA0 1cM0 1cM0 1cM0 1cM0 1cM0 1cM0 1cM0 1cM0 1cM0 1cM0 1cM0 1fA0 1cM0 1cM0 1cM0 1cM0|11e5');
var _elm_community$elm_time$Time_TimeZoneData$australia_brisbane_l = _elm_community$elm_time$Time_TimeZoneData$unpack('Australia/Brisbane|AEST AEDT|-a0 -b0|01010101010101010|-293lX xcX 10jd0 yL0 1cN0 1cL0 1fB0 19X0 17c10 LA0 H1A0 Oo0 1zc0 Oo0 1zc0 Oo0|20e5');
var _elm_community$elm_time$Time_TimeZoneData$australia_broken_hill_l = _elm_community$elm_time$Time_TimeZoneData$unpack('Australia/Broken_Hill|ACST ACDT|-9u -au|0101010101010101010101010101010101010101010101010101010101010101010101010101010101010101010101010101010101010101010101010101010101010101010101|-293lt xcX 10jd0 yL0 1cN0 1cL0 1fB0 19X0 17c10 LA0 1C00 Oo0 1zc0 Oo0 1zc0 Oo0 1zc0 Rc0 1zc0 Oo0 1zc0 Oo0 1zc0 Oo0 1zc0 Oo0 1zc0 Oo0 1zc0 14o0 1o00 Oo0 1zc0 Oo0 1zc0 Oo0 1zc0 U00 1qM0 WM0 1tA0 WM0 1tA0 U00 1tA0 Oo0 1zc0 Oo0 1zc0 Oo0 1zc0 Rc0 1zc0 Oo0 1zc0 Oo0 1zc0 11A0 1o00 11A0 1o00 11A0 1o00 11A0 1qM0 WM0 1qM0 WM0 1qM0 11A0 1o00 11A0 1o00 11A0 1qM0 WM0 1qM0 11A0 1o00 WM0 1qM0 14o0 1cM0 1cM0 1cM0 1cM0 1cM0 1cM0 1cM0 1cM0 1fA0 1cM0 1cM0 1cM0 1cM0 1cM0 1cM0 1cM0 1cM0 1cM0 1cM0 1cM0 1fA0 1cM0 1cM0 1cM0 1cM0 1cM0 1cM0 1cM0 1cM0 1cM0 1cM0 1fA0 1cM0 1cM0 1cM0 1cM0 1cM0 1cM0 1cM0 1cM0 1cM0 1cM0 1fA0 1cM0 1cM0 1cM0 1cM0 1cM0 1cM0 1cM0 1cM0 1cM0 1cM0 1cM0 1fA0 1cM0 1cM0 1cM0 1cM0|18e3');
var _elm_community$elm_time$Time_TimeZoneData$australia_currie_l = _elm_community$elm_time$Time_TimeZoneData$unpack('Australia/Currie|AEST AEDT|-a0 -b0|0101010101010101010101010101010101010101010101010101010101010101010101010101010101010101010101010101010101010101010101010101010101010101010101|-29E80 19X0 10jd0 yL0 1cN0 1cL0 1fB0 19X0 17c10 LA0 1C00 Oo0 1zc0 Oo0 1zc0 Oo0 1zc0 Rc0 1zc0 Oo0 1zc0 Oo0 1zc0 Oo0 1zc0 Oo0 1zc0 Oo0 1zc0 11A0 1qM0 WM0 1qM0 Oo0 1zc0 Oo0 1zc0 Oo0 1wo0 WM0 1tA0 WM0 1tA0 U00 1tA0 U00 1tA0 11A0 1fA0 1a00 1fA0 1a00 1fA0 1a00 1fA0 1a00 1fA0 1cM0 1fA0 1a00 1fA0 1a00 1fA0 1a00 1fA0 1a00 11A0 1o00 1io0 1a00 1fA0 1a00 1fA0 1a00 1fA0 1a00 1fA0 1cM0 1cM0 1a00 1io0 1cM0 1cM0 1cM0 1cM0 1cM0 1cM0 1cM0 1cM0 1cM0 1fA0 1cM0 1cM0 1cM0 1cM0 1cM0 1cM0 1cM0 1cM0 1cM0 1cM0 1cM0 1fA0 1cM0 1cM0 1cM0 1cM0 1cM0 1cM0 1cM0 1cM0 1cM0 1cM0 1fA0 1cM0 1cM0 1cM0 1cM0 1cM0 1cM0 1cM0 1cM0 1cM0 1cM0 1fA0 1cM0 1cM0 1cM0 1cM0 1cM0 1cM0 1cM0 1cM0 1cM0 1cM0 1cM0 1fA0 1cM0 1cM0 1cM0 1cM0|746');
var _elm_community$elm_time$Time_TimeZoneData$australia_darwin_l = _elm_community$elm_time$Time_TimeZoneData$unpack('Australia/Darwin|ACST ACDT|-9u -au|010101010|-293lt xcX 10jd0 yL0 1cN0 1cL0 1fB0 19X0|12e4');
var _elm_community$elm_time$Time_TimeZoneData$australia_eucla_l = _elm_community$elm_time$Time_TimeZoneData$unpack('Australia/Eucla|ACWST ACWDT|-8J -9J|0101010101010101010|-293kI xcX 10jd0 yL0 1cN0 1cL0 1gSp0 Oo0 l5A0 Oo0 iJA0 G00 zU00 IM0 1qM0 11A0 1o00 11A0|368');
var _elm_community$elm_time$Time_TimeZoneData$australia_hobart_l = _elm_community$elm_time$Time_TimeZoneData$unpack('Australia/Hobart|AEST AEDT|-a0 -b0|010101010101010101010101010101010101010101010101010101010101010101010101010101010101010101010101010101010101010101010101010101010101010101010101010101|-29E80 19X0 10jd0 yL0 1cN0 1cL0 1fB0 19X0 VfB0 1cM0 1o00 Rc0 1wo0 Rc0 1wo0 U00 1wo0 LA0 1C00 Oo0 1zc0 Oo0 1zc0 Oo0 1zc0 Rc0 1zc0 Oo0 1zc0 Oo0 1zc0 Oo0 1zc0 Oo0 1zc0 Oo0 1zc0 11A0 1qM0 WM0 1qM0 Oo0 1zc0 Oo0 1zc0 Oo0 1wo0 WM0 1tA0 WM0 1tA0 U00 1tA0 U00 1tA0 11A0 1fA0 1a00 1fA0 1a00 1fA0 1a00 1fA0 1a00 1fA0 1cM0 1fA0 1a00 1fA0 1a00 1fA0 1a00 1fA0 1a00 11A0 1o00 1io0 1a00 1fA0 1a00 1fA0 1a00 1fA0 1a00 1fA0 1cM0 1cM0 1a00 1io0 1cM0 1cM0 1cM0 1cM0 1cM0 1cM0 1cM0 1cM0 1cM0 1fA0 1cM0 1cM0 1cM0 1cM0 1cM0 1cM0 1cM0 1cM0 1cM0 1cM0 1cM0 1fA0 1cM0 1cM0 1cM0 1cM0 1cM0 1cM0 1cM0 1cM0 1cM0 1cM0 1fA0 1cM0 1cM0 1cM0 1cM0 1cM0 1cM0 1cM0 1cM0 1cM0 1cM0 1fA0 1cM0 1cM0 1cM0 1cM0 1cM0 1cM0 1cM0 1cM0 1cM0 1cM0 1cM0 1fA0 1cM0 1cM0 1cM0 1cM0|21e4');
var _elm_community$elm_time$Time_TimeZoneData$australia_lindeman_l = _elm_community$elm_time$Time_TimeZoneData$unpack('Australia/Lindeman|AEST AEDT|-a0 -b0|010101010101010101010|-293lX xcX 10jd0 yL0 1cN0 1cL0 1fB0 19X0 17c10 LA0 H1A0 Oo0 1zc0 Oo0 1zc0 Oo0 1zc0 Rc0 1zc0 Oo0|10');
var _elm_community$elm_time$Time_TimeZoneData$australia_lord_howe_l = _elm_community$elm_time$Time_TimeZoneData$unpack('Australia/Lord_Howe|AEST LHST LHDT LHDT|-a0 -au -bu -b0|0121212121313131313131313131313131313131313131313131313131313131313131313131313131313131313131313131313131313131313|raC0 1zdu Rb0 1zd0 On0 1zd0 On0 1zd0 On0 1zd0 TXu 1qMu WLu 1tAu WLu 1tAu TXu 1tAu Onu 1zcu Onu 1zcu Onu 1zcu Rbu 1zcu Onu 1zcu Onu 1zcu 11zu 1o0u 11zu 1o0u 11zu 1o0u 11zu 1qMu WLu 11Au 1nXu 1qMu 11zu 1o0u 11zu 1o0u 11zu 1qMu WLu 1qMu 11zu 1o0u WLu 1qMu 14nu 1cMu 1cLu 1cMu 1cLu 1cMu 1cLu 1cMu 1cLu 1fAu 1cLu 1cMu 1cLu 1cMu 1cLu 1cMu 1cLu 1cMu 1cLu 1cMu 1cLu 1fAu 1cLu 1cMu 1cLu 1cMu 1cLu 1cMu 1cLu 1cMu 1cLu 1cMu 1fzu 1cMu 1cLu 1cMu 1cLu 1cMu 1cLu 1cMu 1cLu 1cMu 1cLu 1fAu 1cLu 1cMu 1cLu 1cMu 1cLu 1cMu 1cLu 1cMu 1cLu 1cMu 1cLu 1fAu 1cLu 1cMu 1cLu 1cMu|347');
var _elm_community$elm_time$Time_TimeZoneData$australia_melbourne_l = _elm_community$elm_time$Time_TimeZoneData$unpack('Australia/Melbourne|AEST AEDT|-a0 -b0|0101010101010101010101010101010101010101010101010101010101010101010101010101010101010101010101010101010101010101010101010101010101010101010101|-293lX xcX 10jd0 yL0 1cN0 1cL0 1fB0 19X0 17c10 LA0 1C00 Oo0 1zc0 Oo0 1zc0 Oo0 1zc0 Rc0 1zc0 Oo0 1zc0 Oo0 1zc0 Oo0 1zc0 Oo0 1zc0 Oo0 1zc0 Rc0 1zc0 Oo0 1zc0 Oo0 1zc0 Oo0 1zc0 U00 1qM0 WM0 1qM0 11A0 1tA0 U00 1tA0 U00 1tA0 Oo0 1zc0 Oo0 1zc0 Rc0 1zc0 Oo0 1zc0 WM0 1qM0 11A0 1o00 11A0 1o00 11A0 1o00 11A0 1qM0 WM0 11A0 1o00 1qM0 11A0 1o00 11A0 1o00 11A0 1qM0 WM0 1qM0 11A0 1o00 WM0 1qM0 14o0 1cM0 1cM0 1cM0 1cM0 1cM0 1cM0 1cM0 1cM0 1fA0 1cM0 1cM0 1cM0 1cM0 1cM0 1cM0 1cM0 1cM0 1cM0 1cM0 1cM0 1fA0 1cM0 1cM0 1cM0 1cM0 1cM0 1cM0 1cM0 1cM0 1cM0 1cM0 1fA0 1cM0 1cM0 1cM0 1cM0 1cM0 1cM0 1cM0 1cM0 1cM0 1cM0 1fA0 1cM0 1cM0 1cM0 1cM0 1cM0 1cM0 1cM0 1cM0 1cM0 1cM0 1cM0 1fA0 1cM0 1cM0 1cM0 1cM0|39e5');
var _elm_community$elm_time$Time_TimeZoneData$australia_perth_l = _elm_community$elm_time$Time_TimeZoneData$unpack('Australia/Perth|AWST AWDT|-80 -90|0101010101010101010|-293jX xcX 10jd0 yL0 1cN0 1cL0 1gSp0 Oo0 l5A0 Oo0 iJA0 G00 zU00 IM0 1qM0 11A0 1o00 11A0|18e5');
var _elm_community$elm_time$Time_TimeZoneData$australia_sydney_l = _elm_community$elm_time$Time_TimeZoneData$unpack('Australia/Sydney|AEST AEDT|-a0 -b0|0101010101010101010101010101010101010101010101010101010101010101010101010101010101010101010101010101010101010101010101010101010101010101010101|-293lX xcX 10jd0 yL0 1cN0 1cL0 1fB0 19X0 17c10 LA0 1C00 Oo0 1zc0 Oo0 1zc0 Oo0 1zc0 Rc0 1zc0 Oo0 1zc0 Oo0 1zc0 Oo0 1zc0 Oo0 1zc0 Oo0 1zc0 14o0 1o00 Oo0 1zc0 Oo0 1zc0 Oo0 1zc0 U00 1qM0 WM0 1tA0 WM0 1tA0 U00 1tA0 Oo0 1zc0 Oo0 1zc0 Oo0 1zc0 Rc0 1zc0 Oo0 1zc0 Oo0 1zc0 11A0 1o00 11A0 1o00 11A0 1o00 11A0 1qM0 WM0 11A0 1o00 1qM0 11A0 1o00 11A0 1o00 11A0 1qM0 WM0 1qM0 11A0 1o00 WM0 1qM0 14o0 1cM0 1cM0 1cM0 1cM0 1cM0 1cM0 1cM0 1cM0 1fA0 1cM0 1cM0 1cM0 1cM0 1cM0 1cM0 1cM0 1cM0 1cM0 1cM0 1cM0 1fA0 1cM0 1cM0 1cM0 1cM0 1cM0 1cM0 1cM0 1cM0 1cM0 1cM0 1fA0 1cM0 1cM0 1cM0 1cM0 1cM0 1cM0 1cM0 1cM0 1cM0 1cM0 1fA0 1cM0 1cM0 1cM0 1cM0 1cM0 1cM0 1cM0 1cM0 1cM0 1cM0 1cM0 1fA0 1cM0 1cM0 1cM0 1cM0|40e5');
var _elm_community$elm_time$Time_TimeZoneData$cet_l = _elm_community$elm_time$Time_TimeZoneData$unpack('CET|CET CEST|-10 -20|01010101010101010101010101010101010101010101010101010101010101010101010101010101010101010101010101010101010101010101010101010101010101010|-2aFe0 11d0 1iO0 11A0 1o00 11A0 Qrc0 6i00 WM0 1fA0 1cM0 1cM0 1cM0 16M0 1gMM0 1a00 1fA0 1cM0 1cM0 1cM0 1fA0 1a00 1cM0 1cM0 1cM0 1cM0 1cM0 1cM0 1cM0 1fA0 1cM0 1cM0 1cM0 1cM0 1cM0 1cM0 1cM0 1cM0 1cM0 1cM0 1cM0 1fA0 1cM0 1cM0 1cM0 1cM0 1cM0 1cM0 1cM0 1cM0 1cM0 1cM0 1fA0 1o00 11A0 1o00 11A0 1o00 11A0 1qM0 WM0 1qM0 WM0 1qM0 11A0 1o00 11A0 1o00 11A0 1qM0 WM0 1qM0 WM0 1qM0 WM0 1qM0 11A0 1o00 11A0 1o00 11A0 1qM0 WM0 1qM0 WM0 1qM0 11A0 1o00 11A0 1o00 11A0 1o00 11A0 1qM0 WM0 1qM0 WM0 1qM0 11A0 1o00 11A0 1o00 11A0 1qM0 WM0 1qM0 WM0 1qM0 11A0 1o00 11A0 1o00 11A0 1o00 11A0 1qM0 WM0 1qM0 WM0 1qM0 11A0 1o00 11A0 1o00 11A0 1qM0 WM0 1qM0 WM0 1qM0 WM0 1qM0 11A0 1o00 11A0 1o00');
var _elm_community$elm_time$Time_TimeZoneData$cst6cdt_l = _elm_community$elm_time$Time_TimeZoneData$unpack('CST6CDT|CST CDT CWT CPT|60 50 50 50|010102301010101010101010101010101010101010101010101010101010101010101010101010101010101010101010101010101010101010101010101010101010101010101010101010|-261s0 1nX0 11B0 1nX0 SgN0 8x30 iw0 QwN0 1cL0 1cN0 1cL0 1cN0 1cL0 1cN0 1cL0 1cN0 1fz0 1cN0 1cL0 1cN0 1cL0 s10 1Vz0 LB0 1BX0 1cN0 1fz0 1a10 1fz0 1cN0 1cL0 1cN0 1cL0 1cN0 1cL0 1cN0 1cL0 1cN0 1fz0 1a10 1fz0 1cN0 1cL0 1cN0 1cL0 1cN0 1cL0 14p0 1lb0 14p0 1nX0 11B0 1nX0 11B0 1nX0 14p0 1lb0 14p0 1lb0 14p0 1nX0 11B0 1nX0 11B0 1nX0 14p0 1lb0 14p0 1lb0 14p0 1lb0 14p0 1nX0 11B0 1nX0 11B0 1nX0 14p0 1lb0 14p0 1lb0 14p0 1nX0 11B0 1nX0 11B0 1nX0 Rd0 1zb0 Op0 1zb0 Op0 1zb0 Rd0 1zb0 Op0 1zb0 Op0 1zb0 Op0 1zb0 Op0 1zb0 Op0 1zb0 Rd0 1zb0 Op0 1zb0 Op0 1zb0 Op0 1zb0 Op0 1zb0 Rd0 1zb0 Op0 1zb0 Op0 1zb0 Op0 1zb0 Op0 1zb0 Op0 1zb0 Rd0 1zb0 Op0 1zb0 Op0 1zb0 Op0 1zb0 Op0 1zb0 Rd0 1zb0 Op0 1zb0 Op0 1zb0 Op0 1zb0 Op0 1zb0 Op0 1zb0');
var _elm_community$elm_time$Time_TimeZoneData$eet_l = _elm_community$elm_time$Time_TimeZoneData$unpack('EET|EET EEST|-20 -30|010101010101010101010101010101010101010101010101010101010101010101010101010101010101010101010101010101010101010101010101010|hDB0 1a00 1fA0 1cM0 1cM0 1cM0 1fA0 1a00 1cM0 1cM0 1cM0 1cM0 1cM0 1cM0 1cM0 1fA0 1cM0 1cM0 1cM0 1cM0 1cM0 1cM0 1cM0 1cM0 1cM0 1cM0 1cM0 1fA0 1cM0 1cM0 1cM0 1cM0 1cM0 1cM0 1cM0 1cM0 1cM0 1cM0 1fA0 1o00 11A0 1o00 11A0 1o00 11A0 1qM0 WM0 1qM0 WM0 1qM0 11A0 1o00 11A0 1o00 11A0 1qM0 WM0 1qM0 WM0 1qM0 WM0 1qM0 11A0 1o00 11A0 1o00 11A0 1qM0 WM0 1qM0 WM0 1qM0 11A0 1o00 11A0 1o00 11A0 1o00 11A0 1qM0 WM0 1qM0 WM0 1qM0 11A0 1o00 11A0 1o00 11A0 1qM0 WM0 1qM0 WM0 1qM0 11A0 1o00 11A0 1o00 11A0 1o00 11A0 1qM0 WM0 1qM0 WM0 1qM0 11A0 1o00 11A0 1o00 11A0 1qM0 WM0 1qM0 WM0 1qM0 WM0 1qM0 11A0 1o00 11A0 1o00');
var _elm_community$elm_time$Time_TimeZoneData$est5edt_l = _elm_community$elm_time$Time_TimeZoneData$unpack('EST5EDT|EST EDT EWT EPT|50 40 40 40|010102301010101010101010101010101010101010101010101010101010101010101010101010101010101010101010101010101010101010101010101010101010101010101010101010|-261t0 1nX0 11B0 1nX0 SgN0 8x40 iv0 QwN0 1cL0 1cN0 1cL0 1cN0 1cL0 1cN0 1cL0 1cN0 1fz0 1cN0 1cL0 1cN0 1cL0 s10 1Vz0 LB0 1BX0 1cN0 1fz0 1a10 1fz0 1cN0 1cL0 1cN0 1cL0 1cN0 1cL0 1cN0 1cL0 1cN0 1fz0 1a10 1fz0 1cN0 1cL0 1cN0 1cL0 1cN0 1cL0 14p0 1lb0 14p0 1nX0 11B0 1nX0 11B0 1nX0 14p0 1lb0 14p0 1lb0 14p0 1nX0 11B0 1nX0 11B0 1nX0 14p0 1lb0 14p0 1lb0 14p0 1lb0 14p0 1nX0 11B0 1nX0 11B0 1nX0 14p0 1lb0 14p0 1lb0 14p0 1nX0 11B0 1nX0 11B0 1nX0 Rd0 1zb0 Op0 1zb0 Op0 1zb0 Rd0 1zb0 Op0 1zb0 Op0 1zb0 Op0 1zb0 Op0 1zb0 Op0 1zb0 Rd0 1zb0 Op0 1zb0 Op0 1zb0 Op0 1zb0 Op0 1zb0 Rd0 1zb0 Op0 1zb0 Op0 1zb0 Op0 1zb0 Op0 1zb0 Op0 1zb0 Rd0 1zb0 Op0 1zb0 Op0 1zb0 Op0 1zb0 Op0 1zb0 Rd0 1zb0 Op0 1zb0 Op0 1zb0 Op0 1zb0 Op0 1zb0 Op0 1zb0');
var _elm_community$elm_time$Time_TimeZoneData$est_l = _elm_community$elm_time$Time_TimeZoneData$unpack('EST|EST|50|0|');
var _elm_community$elm_time$Time_TimeZoneData$etc_gmt_minus_10_l = _elm_community$elm_time$Time_TimeZoneData$unpack('Etc/GMT-10|+10|-a0|0|');
var _elm_community$elm_time$Time_TimeZoneData$etc_gmt_minus_11_l = _elm_community$elm_time$Time_TimeZoneData$unpack('Etc/GMT-11|+11|-b0|0|');
var _elm_community$elm_time$Time_TimeZoneData$etc_gmt_minus_12_l = _elm_community$elm_time$Time_TimeZoneData$unpack('Etc/GMT-12|+12|-c0|0|');
var _elm_community$elm_time$Time_TimeZoneData$etc_gmt_minus_13_l = _elm_community$elm_time$Time_TimeZoneData$unpack('Etc/GMT-13|+13|-d0|0|');
var _elm_community$elm_time$Time_TimeZoneData$etc_gmt_minus_14_l = _elm_community$elm_time$Time_TimeZoneData$unpack('Etc/GMT-14|+14|-e0|0|');
var _elm_community$elm_time$Time_TimeZoneData$etc_gmt_minus_1_l = _elm_community$elm_time$Time_TimeZoneData$unpack('Etc/GMT-1|+01|-10|0|');
var _elm_community$elm_time$Time_TimeZoneData$etc_gmt_minus_2_l = _elm_community$elm_time$Time_TimeZoneData$unpack('Etc/GMT-2|+02|-20|0|');
var _elm_community$elm_time$Time_TimeZoneData$etc_gmt_minus_3_l = _elm_community$elm_time$Time_TimeZoneData$unpack('Etc/GMT-3|+03|-30|0|');
var _elm_community$elm_time$Time_TimeZoneData$etc_gmt_minus_4_l = _elm_community$elm_time$Time_TimeZoneData$unpack('Etc/GMT-4|+04|-40|0|');
var _elm_community$elm_time$Time_TimeZoneData$etc_gmt_minus_5_l = _elm_community$elm_time$Time_TimeZoneData$unpack('Etc/GMT-5|+05|-50|0|');
var _elm_community$elm_time$Time_TimeZoneData$etc_gmt_minus_6_l = _elm_community$elm_time$Time_TimeZoneData$unpack('Etc/GMT-6|+06|-60|0|');
var _elm_community$elm_time$Time_TimeZoneData$etc_gmt_minus_7_l = _elm_community$elm_time$Time_TimeZoneData$unpack('Etc/GMT-7|+07|-70|0|');
var _elm_community$elm_time$Time_TimeZoneData$etc_gmt_minus_8_l = _elm_community$elm_time$Time_TimeZoneData$unpack('Etc/GMT-8|+08|-80|0|');
var _elm_community$elm_time$Time_TimeZoneData$etc_gmt_minus_9_l = _elm_community$elm_time$Time_TimeZoneData$unpack('Etc/GMT-9|+09|-90|0|');
var _elm_community$elm_time$Time_TimeZoneData$etc_gmt_plus_0_l = _elm_community$elm_time$Time_TimeZoneData$unpack('Etc/GMT+0|GMT|0|0|');
var _elm_community$elm_time$Time_TimeZoneData$etc_gmt_plus_10_l = _elm_community$elm_time$Time_TimeZoneData$unpack('Etc/GMT+10|-10|a0|0|');
var _elm_community$elm_time$Time_TimeZoneData$etc_gmt_plus_11_l = _elm_community$elm_time$Time_TimeZoneData$unpack('Etc/GMT+11|-11|b0|0|');
var _elm_community$elm_time$Time_TimeZoneData$etc_gmt_plus_12_l = _elm_community$elm_time$Time_TimeZoneData$unpack('Etc/GMT+12|-12|c0|0|');
var _elm_community$elm_time$Time_TimeZoneData$etc_gmt_plus_1_l = _elm_community$elm_time$Time_TimeZoneData$unpack('Etc/GMT+1|-01|10|0|');
var _elm_community$elm_time$Time_TimeZoneData$etc_gmt_plus_2_l = _elm_community$elm_time$Time_TimeZoneData$unpack('Etc/GMT+2|-02|20|0|');
var _elm_community$elm_time$Time_TimeZoneData$etc_gmt_plus_3_l = _elm_community$elm_time$Time_TimeZoneData$unpack('Etc/GMT+3|-03|30|0|');
var _elm_community$elm_time$Time_TimeZoneData$etc_gmt_plus_4_l = _elm_community$elm_time$Time_TimeZoneData$unpack('Etc/GMT+4|-04|40|0|');
var _elm_community$elm_time$Time_TimeZoneData$etc_gmt_plus_5_l = _elm_community$elm_time$Time_TimeZoneData$unpack('Etc/GMT+5|-05|50|0|');
var _elm_community$elm_time$Time_TimeZoneData$etc_gmt_plus_6_l = _elm_community$elm_time$Time_TimeZoneData$unpack('Etc/GMT+6|-06|60|0|');
var _elm_community$elm_time$Time_TimeZoneData$etc_gmt_plus_7_l = _elm_community$elm_time$Time_TimeZoneData$unpack('Etc/GMT+7|-07|70|0|');
var _elm_community$elm_time$Time_TimeZoneData$etc_gmt_plus_8_l = _elm_community$elm_time$Time_TimeZoneData$unpack('Etc/GMT+8|-08|80|0|');
var _elm_community$elm_time$Time_TimeZoneData$etc_gmt_plus_9_l = _elm_community$elm_time$Time_TimeZoneData$unpack('Etc/GMT+9|-09|90|0|');
var _elm_community$elm_time$Time_TimeZoneData$etc_uct_l = _elm_community$elm_time$Time_TimeZoneData$unpack('Etc/UCT|UCT|0|0|');
var _elm_community$elm_time$Time_TimeZoneData$etc_utc_l = _elm_community$elm_time$Time_TimeZoneData$unpack('Etc/UTC|UTC|0|0|');
var _elm_community$elm_time$Time_TimeZoneData$europe_amsterdam_l = _elm_community$elm_time$Time_TimeZoneData$unpack('Europe/Amsterdam|AMT NST NEST NET CEST CET|-j.w -1j.w -1k -k -20 -10|010101010101010101010101010101010101010101012323234545454545454545454545454545454545454545454545454545454545454545454545454545454545454545454545454545454545454545454545454545454545|-2aFcj.w 11b0 1iP0 11A0 1io0 1cM0 1fA0 1a00 1fA0 1a00 1fA0 1a00 1co0 1io0 1yo0 Pc0 1a00 1fA0 1Bc0 Mo0 1tc0 Uo0 1tA0 U00 1uo0 W00 1s00 VA0 1so0 Vc0 1sM0 UM0 1wo0 Rc0 1u00 Wo0 1rA0 W00 1s00 VA0 1sM0 UM0 1w00 fV0 BCX.w 1tA0 U00 1u00 Wo0 1sm0 601k WM0 1fA0 1cM0 1cM0 1cM0 16M0 1gMM0 1a00 1fA0 1cM0 1cM0 1cM0 1fA0 1a00 1cM0 1cM0 1cM0 1cM0 1cM0 1cM0 1cM0 1fA0 1cM0 1cM0 1cM0 1cM0 1cM0 1cM0 1cM0 1cM0 1cM0 1cM0 1cM0 1fA0 1cM0 1cM0 1cM0 1cM0 1cM0 1cM0 1cM0 1cM0 1cM0 1cM0 1fA0 1o00 11A0 1o00 11A0 1o00 11A0 1qM0 WM0 1qM0 WM0 1qM0 11A0 1o00 11A0 1o00 11A0 1qM0 WM0 1qM0 WM0 1qM0 WM0 1qM0 11A0 1o00 11A0 1o00 11A0 1qM0 WM0 1qM0 WM0 1qM0 11A0 1o00 11A0 1o00 11A0 1o00 11A0 1qM0 WM0 1qM0 WM0 1qM0 11A0 1o00 11A0 1o00 11A0 1qM0 WM0 1qM0 WM0 1qM0 11A0 1o00 11A0 1o00 11A0 1o00 11A0 1qM0 WM0 1qM0 WM0 1qM0 11A0 1o00 11A0 1o00 11A0 1qM0 WM0 1qM0 WM0 1qM0 WM0 1qM0 11A0 1o00 11A0 1o00|16e5');
var _elm_community$elm_time$Time_TimeZoneData$europe_andorra_l = _elm_community$elm_time$Time_TimeZoneData$unpack('Europe/Andorra|WET CET CEST|0 -10 -20|012121212121212121212121212121212121212121212121212121212121212121212121212121212121212121212121212121212121|-UBA0 1xIN0 1cM0 1cM0 1cM0 1cM0 1cM0 1cM0 1cM0 1cM0 1cM0 1cM0 1fA0 1cM0 1cM0 1cM0 1cM0 1cM0 1cM0 1cM0 1cM0 1cM0 1cM0 1fA0 1o00 11A0 1o00 11A0 1o00 11A0 1qM0 WM0 1qM0 WM0 1qM0 11A0 1o00 11A0 1o00 11A0 1qM0 WM0 1qM0 WM0 1qM0 WM0 1qM0 11A0 1o00 11A0 1o00 11A0 1qM0 WM0 1qM0 WM0 1qM0 11A0 1o00 11A0 1o00 11A0 1o00 11A0 1qM0 WM0 1qM0 WM0 1qM0 11A0 1o00 11A0 1o00 11A0 1qM0 WM0 1qM0 WM0 1qM0 11A0 1o00 11A0 1o00 11A0 1o00 11A0 1qM0 WM0 1qM0 WM0 1qM0 11A0 1o00 11A0 1o00 11A0 1qM0 WM0 1qM0 WM0 1qM0 WM0 1qM0 11A0 1o00 11A0 1o00|79e3');
var _elm_community$elm_time$Time_TimeZoneData$europe_astrakhan_l = _elm_community$elm_time$Time_TimeZoneData$unpack('Europe/Astrakhan|LMT +03 +04 +05|-3c.c -30 -40 -50|012323232323232323212121212121212121212121212121212121212121212|-1Pcrc.c eUMc.c 23CL0 1db0 1cN0 1db0 1cN0 1db0 1dd0 1cO0 1cM0 1cM0 1cM0 1cM0 1cM0 1cM0 1cM0 1cM0 2pB0 1cM0 1fA0 1cM0 3Co0 1cM0 1cM0 1cM0 1cM0 1cM0 1cM0 1fA0 1o00 11A0 1o00 11A0 1o00 11A0 1qM0 WM0 1qM0 WM0 1qM0 11A0 1o00 11A0 1o00 11A0 1qM0 WM0 1qM0 WM0 1qM0 WM0 1qM0 11A0 1o00 11A0 1o00 11A0 1qM0 WM0 8Hz0 3rd0');
var _elm_community$elm_time$Time_TimeZoneData$europe_athens_l = _elm_community$elm_time$Time_TimeZoneData$unpack('Europe/Athens|AMT EET EEST CEST CET|-1y.Q -20 -30 -20 -10|012123434121212121212121212121212121212121212121212121212121212121212121212121212121212121212121212121212121212121212121212121212121212121|-2a61x.Q CNbx.Q mn0 kU10 9b0 3Es0 Xa0 1fb0 1dd0 k3X0 Nz0 SCp0 1vc0 SO0 1cM0 1a00 1ao0 1fc0 1a10 1fG0 1cg0 1dX0 1bX0 1cQ0 1cM0 1cM0 1cM0 1cM0 1cM0 1cM0 1fA0 1cM0 1cM0 1cM0 1cM0 1cM0 1cM0 1cM0 1cM0 1cM0 1cM0 1cM0 1fA0 1cM0 1cM0 1cM0 1cM0 1cM0 1cM0 1cM0 1cM0 1cM0 1cM0 1fA0 1o00 11A0 1o00 11A0 1o00 11A0 1qM0 WM0 1qM0 WM0 1qM0 11A0 1o00 11A0 1o00 11A0 1qM0 WM0 1qM0 WM0 1qM0 WM0 1qM0 11A0 1o00 11A0 1o00 11A0 1qM0 WM0 1qM0 WM0 1qM0 11A0 1o00 11A0 1o00 11A0 1o00 11A0 1qM0 WM0 1qM0 WM0 1qM0 11A0 1o00 11A0 1o00 11A0 1qM0 WM0 1qM0 WM0 1qM0 11A0 1o00 11A0 1o00 11A0 1o00 11A0 1qM0 WM0 1qM0 WM0 1qM0 11A0 1o00 11A0 1o00 11A0 1qM0 WM0 1qM0 WM0 1qM0 WM0 1qM0 11A0 1o00 11A0 1o00|35e5');
var _elm_community$elm_time$Time_TimeZoneData$europe_belgrade_l = _elm_community$elm_time$Time_TimeZoneData$unpack('Europe/Belgrade|CET CEST|-10 -20|01010101010101010101010101010101010101010101010101010101010101010101010101010101010101010101010101010101010101010101010|-19RC0 3IP0 WM0 1fA0 1cM0 1cM0 1rc0 Qo0 1vmo0 1cM0 1cM0 1fA0 1cM0 1cM0 1cM0 1cM0 1cM0 1cM0 1cM0 1cM0 1cM0 1cM0 1cM0 1fA0 1cM0 1cM0 1cM0 1cM0 1cM0 1cM0 1cM0 1cM0 1cM0 1cM0 1fA0 1o00 11A0 1o00 11A0 1o00 11A0 1qM0 WM0 1qM0 WM0 1qM0 11A0 1o00 11A0 1o00 11A0 1qM0 WM0 1qM0 WM0 1qM0 WM0 1qM0 11A0 1o00 11A0 1o00 11A0 1qM0 WM0 1qM0 WM0 1qM0 11A0 1o00 11A0 1o00 11A0 1o00 11A0 1qM0 WM0 1qM0 WM0 1qM0 11A0 1o00 11A0 1o00 11A0 1qM0 WM0 1qM0 WM0 1qM0 11A0 1o00 11A0 1o00 11A0 1o00 11A0 1qM0 WM0 1qM0 WM0 1qM0 11A0 1o00 11A0 1o00 11A0 1qM0 WM0 1qM0 WM0 1qM0 WM0 1qM0 11A0 1o00 11A0 1o00|12e5');
var _elm_community$elm_time$Time_TimeZoneData$europe_berlin_l = _elm_community$elm_time$Time_TimeZoneData$unpack('Europe/Berlin|CET CEST CEMT|-10 -20 -30|01010101010101210101210101010101010101010101010101010101010101010101010101010101010101010101010101010101010101010101010101010101010101010101010|-2aFe0 11d0 1iO0 11A0 1o00 11A0 Qrc0 6i00 WM0 1fA0 1cM0 1cM0 1cM0 kL0 Nc0 m10 WM0 1ao0 1cp0 dX0 jz0 Dd0 1io0 17c0 1fA0 1a00 1ehA0 1a00 1cM0 1cM0 1cM0 1cM0 1cM0 1cM0 1cM0 1fA0 1cM0 1cM0 1cM0 1cM0 1cM0 1cM0 1cM0 1cM0 1cM0 1cM0 1cM0 1fA0 1cM0 1cM0 1cM0 1cM0 1cM0 1cM0 1cM0 1cM0 1cM0 1cM0 1fA0 1o00 11A0 1o00 11A0 1o00 11A0 1qM0 WM0 1qM0 WM0 1qM0 11A0 1o00 11A0 1o00 11A0 1qM0 WM0 1qM0 WM0 1qM0 WM0 1qM0 11A0 1o00 11A0 1o00 11A0 1qM0 WM0 1qM0 WM0 1qM0 11A0 1o00 11A0 1o00 11A0 1o00 11A0 1qM0 WM0 1qM0 WM0 1qM0 11A0 1o00 11A0 1o00 11A0 1qM0 WM0 1qM0 WM0 1qM0 11A0 1o00 11A0 1o00 11A0 1o00 11A0 1qM0 WM0 1qM0 WM0 1qM0 11A0 1o00 11A0 1o00 11A0 1qM0 WM0 1qM0 WM0 1qM0 WM0 1qM0 11A0 1o00 11A0 1o00|41e5');
var _elm_community$elm_time$Time_TimeZoneData$europe_brussels_l = _elm_community$elm_time$Time_TimeZoneData$unpack('Europe/Brussels|WET CET CEST WEST|0 -10 -20 -10|0121212103030303030303030303030303030303030303030303212121212121212121212121212121212121212121212121212121212121212121212121212121212121212121212121212121212121212121212121212121212121|-2ehc0 3zX0 11c0 1iO0 11A0 1o00 11A0 my0 Ic0 1qM0 Rc0 1EM0 UM0 1u00 10o0 1io0 1io0 17c0 1a00 1fA0 1cM0 1cM0 1io0 17c0 1fA0 1a00 1io0 1a30 1io0 17c0 1fA0 1a00 1io0 17c0 1cM0 1cM0 1a00 1io0 1cM0 1cM0 1a00 1fA0 1io0 17c0 1cM0 1cM0 1a00 1fA0 1io0 1qM0 Dc0 y00 5Wn0 WM0 1fA0 1cM0 16M0 1iM0 16M0 1C00 Uo0 1eeo0 1a00 1fA0 1cM0 1cM0 1cM0 1fA0 1a00 1cM0 1cM0 1cM0 1cM0 1cM0 1cM0 1cM0 1fA0 1cM0 1cM0 1cM0 1cM0 1cM0 1cM0 1cM0 1cM0 1cM0 1cM0 1cM0 1fA0 1cM0 1cM0 1cM0 1cM0 1cM0 1cM0 1cM0 1cM0 1cM0 1cM0 1fA0 1o00 11A0 1o00 11A0 1o00 11A0 1qM0 WM0 1qM0 WM0 1qM0 11A0 1o00 11A0 1o00 11A0 1qM0 WM0 1qM0 WM0 1qM0 WM0 1qM0 11A0 1o00 11A0 1o00 11A0 1qM0 WM0 1qM0 WM0 1qM0 11A0 1o00 11A0 1o00 11A0 1o00 11A0 1qM0 WM0 1qM0 WM0 1qM0 11A0 1o00 11A0 1o00 11A0 1qM0 WM0 1qM0 WM0 1qM0 11A0 1o00 11A0 1o00 11A0 1o00 11A0 1qM0 WM0 1qM0 WM0 1qM0 11A0 1o00 11A0 1o00 11A0 1qM0 WM0 1qM0 WM0 1qM0 WM0 1qM0 11A0 1o00 11A0 1o00|21e5');
var _elm_community$elm_time$Time_TimeZoneData$europe_bucharest_l = _elm_community$elm_time$Time_TimeZoneData$unpack('Europe/Bucharest|BMT EET EEST|-1I.o -20 -30|0121212121212121212121212121212121212121212121212121212121212121212121212121212121212121212121212121212121212121212121212121212121212121|-1xApI.o 20LI.o RA0 1cM0 1cM0 1fA0 1cM0 1cM0 1cM0 1cM0 1cM0 1cM0 1cM0 1cM0 1cM0 1cM0 1cM0 1Axc0 On0 1fA0 1a10 1cO0 1cM0 1cM0 1cM0 1cM0 1cM0 1cM0 1fA0 1cM0 1cM0 1cM0 1cM0 1cM0 1cM0 1cM0 1cM0 1cM0 1cM0 1cM0 1fA0 1cK0 1cM0 1cM0 1cM0 1cM0 1cM0 1cM0 1cL0 1cN0 1cL0 1fB0 1nX0 11E0 1o00 11A0 1o00 11A0 1qM0 WM0 1qM0 WM0 1qM0 11A0 1o00 11A0 1o00 11A0 1qM0 WM0 1qM0 WM0 1qM0 WM0 1qM0 11A0 1o00 11A0 1o00 11A0 1qM0 WM0 1qM0 WM0 1qM0 11A0 1o00 11A0 1o00 11A0 1o00 11A0 1qM0 WM0 1qM0 WM0 1qM0 11A0 1o00 11A0 1o00 11A0 1qM0 WM0 1qM0 WM0 1qM0 11A0 1o00 11A0 1o00 11A0 1o00 11A0 1qM0 WM0 1qM0 WM0 1qM0 11A0 1o00 11A0 1o00 11A0 1qM0 WM0 1qM0 WM0 1qM0 WM0 1qM0 11A0 1o00 11A0 1o00|19e5');
var _elm_community$elm_time$Time_TimeZoneData$europe_budapest_l = _elm_community$elm_time$Time_TimeZoneData$unpack('Europe/Budapest|CET CEST|-10 -20|0101010101010101010101010101010101010101010101010101010101010101010101010101010101010101010101010101010101010101010101010101010101010101010101010101010|-2aFe0 11d0 1iO0 11A0 1ip0 17b0 1op0 1tb0 Q2m0 3Ne0 WM0 1fA0 1cM0 1cM0 1oJ0 1dc0 1030 1fA0 1cM0 1cM0 1cM0 1cM0 1fA0 1a00 1iM0 1fA0 8Ha0 Rb0 1wN0 Rb0 1BB0 Lz0 1C20 LB0 SNX0 1a10 1cM0 1cM0 1cM0 1cM0 1cM0 1cM0 1cM0 1fA0 1cM0 1cM0 1cM0 1cM0 1cM0 1cM0 1cM0 1cM0 1cM0 1cM0 1cM0 1fA0 1cM0 1cM0 1cM0 1cM0 1cM0 1cM0 1cM0 1cM0 1cM0 1cM0 1fA0 1o00 11A0 1o00 11A0 1o00 11A0 1qM0 WM0 1qM0 WM0 1qM0 11A0 1o00 11A0 1o00 11A0 1qM0 WM0 1qM0 WM0 1qM0 WM0 1qM0 11A0 1o00 11A0 1o00 11A0 1qM0 WM0 1qM0 WM0 1qM0 11A0 1o00 11A0 1o00 11A0 1o00 11A0 1qM0 WM0 1qM0 WM0 1qM0 11A0 1o00 11A0 1o00 11A0 1qM0 WM0 1qM0 WM0 1qM0 11A0 1o00 11A0 1o00 11A0 1o00 11A0 1qM0 WM0 1qM0 WM0 1qM0 11A0 1o00 11A0 1o00 11A0 1qM0 WM0 1qM0 WM0 1qM0 WM0 1qM0 11A0 1o00 11A0 1o00|17e5');
var _elm_community$elm_time$Time_TimeZoneData$europe_chisinau_l = _elm_community$elm_time$Time_TimeZoneData$unpack('Europe/Chisinau|CMT BMT EET EEST CEST CET MSK MSD|-1T -1I.o -20 -30 -20 -10 -30 -40|012323232323232323234545467676767676767676767323232323232323232323232323232323232323232323232323232323232323232323232323232323232323232323232|-26jdT wGMa.A 20LI.o RA0 1cM0 1cM0 1fA0 1cM0 1cM0 1cM0 1cM0 1cM0 1cM0 1cM0 1cM0 1cM0 1cM0 1cM0 27A0 2en0 39g0 WM0 1fA0 1cM0 V90 1t7z0 1db0 1cN0 1db0 1cN0 1db0 1dd0 1cO0 1cM0 1cM0 1cM0 1cM0 1cM0 1cM0 1cM0 1cM0 1cM0 1cM0 1cM0 gL0 WO0 1cM0 1cM0 1cK0 1cL0 1cN0 1cL0 1cN0 1cL0 1cN0 1cL0 1fB0 1nX0 11D0 1o00 11A0 1o00 11A0 1qM0 WM0 1qM0 WM0 1qM0 11A0 1o00 11A0 1o00 11A0 1qM0 WM0 1qM0 WM0 1qM0 WM0 1qM0 11A0 1o00 11A0 1o00 11A0 1qM0 WM0 1qM0 WM0 1qM0 11A0 1o00 11A0 1o00 11A0 1o00 11A0 1qM0 WM0 1qM0 WM0 1qM0 11A0 1o00 11A0 1o00 11A0 1qM0 WM0 1qM0 WM0 1qM0 11A0 1o00 11A0 1o00 11A0 1o00 11A0 1qM0 WM0 1qM0 WM0 1qM0 11A0 1o00 11A0 1o00 11A0 1qM0 WM0 1qM0 WM0 1qM0 WM0 1qM0 11A0 1o00 11A0 1o00|67e4');
var _elm_community$elm_time$Time_TimeZoneData$europe_copenhagen_l = _elm_community$elm_time$Time_TimeZoneData$unpack('Europe/Copenhagen|CET CEST|-10 -20|0101010101010101010101010101010101010101010101010101010101010101010101010101010101010101010101010101010101010101010101010101010101010|-2azC0 Tz0 VuO0 60q0 WM0 1fA0 1cM0 1cM0 1cM0 S00 1HA0 Nc0 1C00 Dc0 1Nc0 Ao0 1h5A0 1a00 1cM0 1cM0 1cM0 1cM0 1cM0 1cM0 1cM0 1fA0 1cM0 1cM0 1cM0 1cM0 1cM0 1cM0 1cM0 1cM0 1cM0 1cM0 1cM0 1fA0 1cM0 1cM0 1cM0 1cM0 1cM0 1cM0 1cM0 1cM0 1cM0 1cM0 1fA0 1o00 11A0 1o00 11A0 1o00 11A0 1qM0 WM0 1qM0 WM0 1qM0 11A0 1o00 11A0 1o00 11A0 1qM0 WM0 1qM0 WM0 1qM0 WM0 1qM0 11A0 1o00 11A0 1o00 11A0 1qM0 WM0 1qM0 WM0 1qM0 11A0 1o00 11A0 1o00 11A0 1o00 11A0 1qM0 WM0 1qM0 WM0 1qM0 11A0 1o00 11A0 1o00 11A0 1qM0 WM0 1qM0 WM0 1qM0 11A0 1o00 11A0 1o00 11A0 1o00 11A0 1qM0 WM0 1qM0 WM0 1qM0 11A0 1o00 11A0 1o00 11A0 1qM0 WM0 1qM0 WM0 1qM0 WM0 1qM0 11A0 1o00 11A0 1o00|12e5');
var _elm_community$elm_time$Time_TimeZoneData$europe_dublin_l = _elm_community$elm_time$Time_TimeZoneData$unpack('Europe/Dublin|DMT IST GMT BST IST|p.l -y.D 0 -10 -10|01232323232324242424242424242424242424242424242424242424242424242424242424242424242424242424242424242424242424242424242424242424242424242424242424242424242424242424242424242424242424242424242424242424242424242424242424242424242|-2ax9y.D Rc0 1fzy.D 14M0 1fc0 1g00 1co0 1dc0 1co0 1oo0 1400 1dc0 19A0 1io0 1io0 WM0 1o00 14o0 1o00 17c0 1io0 17c0 1fA0 1a00 1lc0 17c0 1io0 17c0 1fA0 1a00 1io0 17c0 1io0 17c0 1fA0 1cM0 1io0 17c0 1fA0 1a00 1io0 17c0 1io0 17c0 1fA0 1a00 1io0 1qM0 Dc0 g5X0 14p0 1wn0 17d0 1io0 11A0 1o00 17c0 1fA0 1a00 1fA0 1cM0 1fA0 1a00 17c0 1fA0 1a00 1io0 17c0 1lc0 17c0 1fA0 1a00 1io0 17c0 1io0 17c0 1fA0 1a00 1a00 1qM0 WM0 1qM0 11A0 1o00 WM0 1qM0 WM0 1qM0 WM0 1qM0 WM0 1tA0 IM0 90o0 U00 1tA0 U00 1tA0 U00 1tA0 U00 1tA0 WM0 1qM0 WM0 1qM0 WM0 1tA0 U00 1tA0 U00 1tA0 11z0 1o00 11A0 1o00 11A0 1o00 11A0 1qM0 11A0 1o00 11A0 1o00 11A0 1o00 11A0 1o00 11A0 1qM0 WM0 1qM0 11A0 1o00 11A0 1o00 11A0 1o00 11A0 1o00 11A0 1o00 14o0 1o00 11A0 1o00 11A0 1o00 11A0 1qM0 WM0 1qM0 WM0 1qM0 11A0 1o00 11A0 1o00 11A0 1qM0 WM0 1qM0 WM0 1qM0 WM0 1qM0 11A0 1o00 11A0 1o00 11A0 1qM0 WM0 1qM0 WM0 1qM0 11A0 1o00 11A0 1o00 11A0 1o00 11A0 1qM0 WM0 1qM0 WM0 1qM0 11A0 1o00 11A0 1o00 11A0 1qM0 WM0 1qM0 WM0 1qM0 11A0 1o00 11A0 1o00 11A0 1o00 11A0 1qM0 WM0 1qM0 WM0 1qM0 11A0 1o00 11A0 1o00 11A0 1qM0 WM0 1qM0 WM0 1qM0 WM0 1qM0 11A0 1o00 11A0 1o00|12e5');
var _elm_community$elm_time$Time_TimeZoneData$europe_gibraltar_l = _elm_community$elm_time$Time_TimeZoneData$unpack('Europe/Gibraltar|GMT BST BDST CET CEST|0 -10 -20 -10 -20|010101010101010101010101010101010101010101010101012121212121010121010101010101010101034343434343434343434343434343434343434343434343434343434343434343434343434343434343434343434343434343434343434343|-2axa0 Rc0 1fA0 14M0 1fc0 1g00 1co0 1dc0 1co0 1oo0 1400 1dc0 19A0 1io0 1io0 WM0 1o00 14o0 1o00 17c0 1io0 17c0 1fA0 1a00 1lc0 17c0 1io0 17c0 1fA0 1a00 1io0 17c0 1io0 17c0 1fA0 1cM0 1io0 17c0 1fA0 1a00 1io0 17c0 1io0 17c0 1fA0 1a00 1io0 1qM0 Dc0 2Rz0 Dc0 1zc0 Oo0 1zc0 Rc0 1wo0 17c0 1iM0 FA0 xB0 1fA0 1a00 14o0 bb0 LA0 xB0 Rc0 1wo0 11A0 1o00 17c0 1fA0 1a00 1fA0 1cM0 1fA0 1a00 17c0 1fA0 1a00 1io0 17c0 1lc0 17c0 1fA0 10Jz0 1cM0 1cM0 1cM0 1cM0 1fA0 1cM0 1cM0 1cM0 1cM0 1cM0 1cM0 1cM0 1cM0 1cM0 1cM0 1cM0 1fA0 1cM0 1cM0 1cM0 1cM0 1cM0 1cM0 1cM0 1cM0 1cM0 1cM0 1fA0 1o00 11A0 1o00 11A0 1o00 11A0 1qM0 WM0 1qM0 WM0 1qM0 11A0 1o00 11A0 1o00 11A0 1qM0 WM0 1qM0 WM0 1qM0 WM0 1qM0 11A0 1o00 11A0 1o00 11A0 1qM0 WM0 1qM0 WM0 1qM0 11A0 1o00 11A0 1o00 11A0 1o00 11A0 1qM0 WM0 1qM0 WM0 1qM0 11A0 1o00 11A0 1o00 11A0 1qM0 WM0 1qM0 WM0 1qM0 11A0 1o00 11A0 1o00 11A0 1o00 11A0 1qM0 WM0 1qM0 WM0 1qM0 11A0 1o00 11A0 1o00 11A0 1qM0 WM0 1qM0 WM0 1qM0 WM0 1qM0 11A0 1o00 11A0 1o00|30e3');
var _elm_community$elm_time$Time_TimeZoneData$europe_helsinki_l = _elm_community$elm_time$Time_TimeZoneData$unpack('Europe/Helsinki|HMT EET EEST|-1D.N -20 -30|0121212121212121212121212121212121212121212121212121212121212121212121212121212121212121212121212121212121212121212121|-1WuND.N OULD.N 1dA0 1xGq0 1cM0 1cM0 1cM0 1cN0 1cM0 1cM0 1fA0 1cM0 1cM0 1cM0 1cM0 1cM0 1cM0 1cM0 1cM0 1cM0 1cM0 1cM0 1fA0 1cM0 1cM0 1cM0 1cM0 1cM0 1cM0 1cM0 1cM0 1cM0 1cM0 1fA0 1o00 11A0 1o00 11A0 1o00 11A0 1qM0 WM0 1qM0 WM0 1qM0 11A0 1o00 11A0 1o00 11A0 1qM0 WM0 1qM0 WM0 1qM0 WM0 1qM0 11A0 1o00 11A0 1o00 11A0 1qM0 WM0 1qM0 WM0 1qM0 11A0 1o00 11A0 1o00 11A0 1o00 11A0 1qM0 WM0 1qM0 WM0 1qM0 11A0 1o00 11A0 1o00 11A0 1qM0 WM0 1qM0 WM0 1qM0 11A0 1o00 11A0 1o00 11A0 1o00 11A0 1qM0 WM0 1qM0 WM0 1qM0 11A0 1o00 11A0 1o00 11A0 1qM0 WM0 1qM0 WM0 1qM0 WM0 1qM0 11A0 1o00 11A0 1o00|12e5');
var _elm_community$elm_time$Time_TimeZoneData$europe_istanbul_l = _elm_community$elm_time$Time_TimeZoneData$unpack('Europe/Istanbul|IMT EET EEST +04 +03|-1U.U -20 -30 -40 -30|012121212121212121212121212121212121212121212121212121234343434342121212121212121212121212121212121212121212121212121212121212124|-2ogNU.U dzzU.U 11b0 8tB0 1on0 1410 1db0 19B0 1in0 3Rd0 Un0 1oN0 11b0 zSp0 CL0 mN0 1Vz0 1gN0 1pz0 5Rd0 1fz0 1yp0 ML0 1kp0 17b0 1ip0 17b0 1fB0 19X0 1jB0 18L0 1ip0 17z0 qdd0 xX0 3S10 Tz0 dA10 11z0 1o10 11z0 1qN0 11z0 1ze0 11B0 WM0 1qO0 WI0 1nX0 1rB0 10L0 11B0 1in0 17d0 1in0 2pX0 19E0 1fU0 16Q0 1iI0 16Q0 1iI0 1Vd0 pb0 3Kp0 14o0 1de0 1cM0 1cM0 1cM0 1cM0 1cM0 1cM0 1cM0 1cM0 1fA0 1cM0 1cM0 1cM0 1cM0 1cM0 1cM0 1a00 1fA0 1cM0 1cM0 1fA0 1o00 11A0 1o00 11A0 1o00 11A0 1qM0 WM0 1qM0 WM0 1qM0 11A0 1o00 11A0 1o00 11A0 1qM0 WM0 1qM0 WM0 1qM0 WO0 1qM0 11A0 1o00 11A0 1o00 11A0 1qM0 Xc0 1qo0 WM0 1qM0 11A0 1o00 1200 1nA0 11A0 1tA0 U00 15w0|13e6');
var _elm_community$elm_time$Time_TimeZoneData$europe_kaliningrad_l = _elm_community$elm_time$Time_TimeZoneData$unpack('Europe/Kaliningrad|CET CEST CET CEST MSK MSD EEST EET +03|-10 -20 -20 -30 -30 -40 -30 -20 -30|0101010101010232454545454545454546767676767676767676767676767676767676767676787|-2aFe0 11d0 1iO0 11A0 1o00 11A0 Qrc0 6i00 WM0 1fA0 1cM0 1cM0 Am0 Lb0 1en0 op0 1pNz0 1db0 1cN0 1db0 1cN0 1db0 1dd0 1cO0 1cM0 1cM0 1cM0 1cM0 1cM0 1cM0 1cM0 1cM0 1cM0 1cN0 1cM0 1fA0 1cM0 1cM0 1cM0 1cM0 1cM0 1cM0 1cM0 1cM0 1cM0 1cM0 1fA0 1o00 11A0 1o00 11A0 1o00 11A0 1qM0 WM0 1qM0 WM0 1qM0 11A0 1o00 11A0 1o00 11A0 1qM0 WM0 1qM0 WM0 1qM0 WM0 1qM0 11A0 1o00 11A0 1o00 11A0 1qM0 WM0 8Hz0|44e4');
var _elm_community$elm_time$Time_TimeZoneData$europe_kiev_l = _elm_community$elm_time$Time_TimeZoneData$unpack('Europe/Kiev|KMT EET MSK CEST CET MSD EEST|-22.4 -20 -30 -20 -10 -40 -30|0123434252525252525252525256161616161616161616161616161616161616161616161616161616161616161616161616161616161616161616161|-1Pc22.4 eUo2.4 rnz0 2Hg0 WM0 1fA0 da0 1v4m0 1db0 1cN0 1db0 1cN0 1db0 1dd0 1cO0 1cM0 1cM0 1cM0 1cM0 1cM0 1cM0 1cM0 1cM0 1cM0 1cM0 1cM0 Db0 3220 1cK0 1cL0 1cN0 1cL0 1cN0 1cL0 1cQ0 1cM0 1fA0 1o00 11A0 1o00 11A0 1o00 11A0 1qM0 WM0 1qM0 WM0 1qM0 11A0 1o00 11A0 1o00 11A0 1qM0 WM0 1qM0 WM0 1qM0 WM0 1qM0 11A0 1o00 11A0 1o00 11A0 1qM0 WM0 1qM0 WM0 1qM0 11A0 1o00 11A0 1o00 11A0 1o00 11A0 1qM0 WM0 1qM0 WM0 1qM0 11A0 1o00 11A0 1o00 11A0 1qM0 WM0 1qM0 WM0 1qM0 11A0 1o00 11A0 1o00 11A0 1o00 11A0 1qM0 WM0 1qM0 WM0 1qM0 11A0 1o00 11A0 1o00 11A0 1qM0 WM0 1qM0 WM0 1qM0 WM0 1qM0 11A0 1o00 11A0 1o00|34e5');
var _elm_community$elm_time$Time_TimeZoneData$europe_kirov_l = _elm_community$elm_time$Time_TimeZoneData$unpack('Europe/Kirov|LMT +03 +04 +05|-3i.M -30 -40 -50|01232323232323232321212121212121212121212121212121212121212121|-22WM0 qH90 23CL0 1db0 1cN0 1db0 1cN0 1db0 1dd0 1cO0 1cM0 1cM0 1cM0 1cM0 1cM0 1cM0 1cM0 1cM0 2pB0 1cM0 1fA0 1cM0 3Co0 1cM0 1cM0 1cM0 1cM0 1cM0 1cM0 1fA0 1o00 11A0 1o00 11A0 1o00 11A0 1qM0 WM0 1qM0 WM0 1qM0 11A0 1o00 11A0 1o00 11A0 1qM0 WM0 1qM0 WM0 1qM0 WM0 1qM0 11A0 1o00 11A0 1o00 11A0 1qM0 WM0 8Hz0|48e4');
var _elm_community$elm_time$Time_TimeZoneData$europe_lisbon_l = _elm_community$elm_time$Time_TimeZoneData$unpack('Europe/Lisbon|LMT WET WEST WEMT CET CEST|A.J 0 -10 -20 -10 -20|012121212121212121212121212121212121212121212321232123212321212121212121212121212121212121212121214121212121212121212121212121212124545454212121212121212121212121212121212121212121212121212121212121212121212121212121212121|-2ldXn.f aPWn.f Sp0 LX0 1vc0 Tc0 1uM0 SM0 1vc0 Tc0 1vc0 SM0 1vc0 6600 1co0 3E00 17c0 1fA0 1a00 1io0 1a00 1io0 17c0 3I00 17c0 1cM0 1cM0 3Fc0 1cM0 1a00 1fA0 1io0 17c0 1cM0 1cM0 1a00 1fA0 1io0 1qM0 Dc0 1tA0 1cM0 1dc0 1400 gL0 IM0 s10 U00 dX0 Rc0 pd0 Rc0 gL0 Oo0 pd0 Rc0 gL0 Oo0 pd0 14o0 1cM0 1cP0 1cM0 1cM0 1cM0 1cM0 1cM0 3Co0 1fA0 1cM0 1cM0 1cM0 1cM0 1cM0 1cM0 1cM0 1cM0 1cM0 1fA0 1cM0 1cM0 1cM0 1cM0 1cM0 1cM0 1cM0 1cM0 1cM0 1cM0 1cM0 1fA0 1cM0 1cM0 1cM0 1cM0 1cM0 1cM0 1cM0 pvy0 1cM0 1cM0 1fA0 1cM0 1cM0 1cN0 1cL0 1cN0 1cM0 1cM0 1cM0 1cM0 1cN0 1cL0 1cM0 1fA0 1cM0 1cM0 1cM0 1cM0 1cM0 1cM0 1cM0 1cM0 1cM0 1cM0 1cM0 1fA0 1cM0 1cM0 1cM0 1cM0 1cM0 1cM0 1cM0 1cM0 1cM0 1cM0 1fA0 1o00 11A0 1o00 11A0 1o00 11A0 1qM0 WM0 1qM0 WM0 1qM0 11A0 1o00 11A0 1o00 11A0 1qM0 WM0 1qM0 WM0 1qM0 WM0 1qM0 11A0 1o00 11A0 1o00 11A0 1qM0 WM0 1qM0 WM0 1qM0 11A0 1o00 11A0 1o00 11A0 1o00 11A0 1qM0 WM0 1qM0 WM0 1qM0 11A0 1o00 11A0 1o00 11A0 1qM0 WM0 1qM0 WM0 1qM0 11A0 1o00 11A0 1o00 11A0 1o00 11A0 1qM0 WM0 1qM0 WM0 1qM0 11A0 1o00 11A0 1o00 11A0 1qM0 WM0 1qM0 WM0 1qM0 WM0 1qM0 11A0 1o00 11A0 1o00|27e5');
var _elm_community$elm_time$Time_TimeZoneData$europe_london_l = _elm_community$elm_time$Time_TimeZoneData$unpack('Europe/London|GMT BST BDST|0 -10 -20|0101010101010101010101010101010101010101010101010121212121210101210101010101010101010101010101010101010101010101010101010101010101010101010101010101010101010101010101010101010101010101010101010101010101010101010101010101010101010101010101010|-2axa0 Rc0 1fA0 14M0 1fc0 1g00 1co0 1dc0 1co0 1oo0 1400 1dc0 19A0 1io0 1io0 WM0 1o00 14o0 1o00 17c0 1io0 17c0 1fA0 1a00 1lc0 17c0 1io0 17c0 1fA0 1a00 1io0 17c0 1io0 17c0 1fA0 1cM0 1io0 17c0 1fA0 1a00 1io0 17c0 1io0 17c0 1fA0 1a00 1io0 1qM0 Dc0 2Rz0 Dc0 1zc0 Oo0 1zc0 Rc0 1wo0 17c0 1iM0 FA0 xB0 1fA0 1a00 14o0 bb0 LA0 xB0 Rc0 1wo0 11A0 1o00 17c0 1fA0 1a00 1fA0 1cM0 1fA0 1a00 17c0 1fA0 1a00 1io0 17c0 1lc0 17c0 1fA0 1a00 1io0 17c0 1io0 17c0 1fA0 1a00 1a00 1qM0 WM0 1qM0 11A0 1o00 WM0 1qM0 WM0 1qM0 WM0 1qM0 WM0 1tA0 IM0 90o0 U00 1tA0 U00 1tA0 U00 1tA0 U00 1tA0 WM0 1qM0 WM0 1qM0 WM0 1tA0 U00 1tA0 U00 1tA0 11z0 1o00 11A0 1o00 11A0 1o00 11A0 1qM0 11A0 1o00 11A0 1o00 11A0 1o00 11A0 1o00 11A0 1qM0 WM0 1qM0 11A0 1o00 11A0 1o00 11A0 1o00 11A0 1o00 11A0 1o00 14o0 1o00 11A0 1o00 11A0 1o00 11A0 1qM0 WM0 1qM0 WM0 1qM0 11A0 1o00 11A0 1o00 11A0 1qM0 WM0 1qM0 WM0 1qM0 WM0 1qM0 11A0 1o00 11A0 1o00 11A0 1qM0 WM0 1qM0 WM0 1qM0 11A0 1o00 11A0 1o00 11A0 1o00 11A0 1qM0 WM0 1qM0 WM0 1qM0 11A0 1o00 11A0 1o00 11A0 1qM0 WM0 1qM0 WM0 1qM0 11A0 1o00 11A0 1o00 11A0 1o00 11A0 1qM0 WM0 1qM0 WM0 1qM0 11A0 1o00 11A0 1o00 11A0 1qM0 WM0 1qM0 WM0 1qM0 WM0 1qM0 11A0 1o00 11A0 1o00|10e6');
var _elm_community$elm_time$Time_TimeZoneData$europe_luxembourg_l = _elm_community$elm_time$Time_TimeZoneData$unpack('Europe/Luxembourg|LMT CET CEST WET WEST WEST WET|-o.A -10 -20 0 -10 -20 -10|0121212134343434343434343434343434343434343434343434565651212121212121212121212121212121212121212121212121212121212121212121212121212121212121212121212121212121212121212121212121212121|-2DG0o.A t6mo.A TB0 1nX0 Up0 1o20 11A0 rW0 CM0 1qP0 R90 1EO0 UK0 1u20 10m0 1ip0 1in0 17e0 19W0 1fB0 1db0 1cp0 1in0 17d0 1fz0 1a10 1in0 1a10 1in0 17f0 1fA0 1a00 1io0 17c0 1cM0 1cM0 1a00 1io0 1cM0 1cM0 1a00 1fA0 1io0 17c0 1cM0 1cM0 1a00 1fA0 1io0 1qM0 Dc0 vA0 60L0 WM0 1fA0 1cM0 17c0 1io0 16M0 1C00 Uo0 1eeo0 1a00 1fA0 1cM0 1cM0 1cM0 1fA0 1a00 1cM0 1cM0 1cM0 1cM0 1cM0 1cM0 1cM0 1fA0 1cM0 1cM0 1cM0 1cM0 1cM0 1cM0 1cM0 1cM0 1cM0 1cM0 1cM0 1fA0 1cM0 1cM0 1cM0 1cM0 1cM0 1cM0 1cM0 1cM0 1cM0 1cM0 1fA0 1o00 11A0 1o00 11A0 1o00 11A0 1qM0 WM0 1qM0 WM0 1qM0 11A0 1o00 11A0 1o00 11A0 1qM0 WM0 1qM0 WM0 1qM0 WM0 1qM0 11A0 1o00 11A0 1o00 11A0 1qM0 WM0 1qM0 WM0 1qM0 11A0 1o00 11A0 1o00 11A0 1o00 11A0 1qM0 WM0 1qM0 WM0 1qM0 11A0 1o00 11A0 1o00 11A0 1qM0 WM0 1qM0 WM0 1qM0 11A0 1o00 11A0 1o00 11A0 1o00 11A0 1qM0 WM0 1qM0 WM0 1qM0 11A0 1o00 11A0 1o00 11A0 1qM0 WM0 1qM0 WM0 1qM0 WM0 1qM0 11A0 1o00 11A0 1o00|54e4');
var _elm_community$elm_time$Time_TimeZoneData$europe_madrid_l = _elm_community$elm_time$Time_TimeZoneData$unpack('Europe/Madrid|WET WEST WEMT CET CEST|0 -10 -20 -10 -20|01010101010101010101010121212121234343434343434343434343434343434343434343434343434343434343434343434343434343434343434343434343434343434343434343434343434343434343|-28dd0 11A0 1go0 19A0 1co0 1dA0 b1A0 18o0 3I00 17c0 1fA0 1a00 1io0 1a00 1io0 17c0 iyo0 Rc0 18o0 1hc0 1io0 1a00 14o0 5aL0 MM0 1vc0 17A0 1i00 1bc0 1eo0 17d0 1in0 17A0 6hA0 10N0 XIL0 1a10 1in0 17d0 19X0 1cN0 1fz0 1a10 1fX0 1cp0 1cO0 1cM0 1fA0 1a00 1cM0 1cM0 1cM0 1cM0 1cM0 1cM0 1cM0 1fA0 1cM0 1cM0 1cM0 1cM0 1cM0 1cM0 1cM0 1cM0 1cM0 1cM0 1cM0 1fA0 1cM0 1cM0 1cM0 1cM0 1cM0 1cM0 1cM0 1cM0 1cM0 1cM0 1fA0 1o00 11A0 1o00 11A0 1o00 11A0 1qM0 WM0 1qM0 WM0 1qM0 11A0 1o00 11A0 1o00 11A0 1qM0 WM0 1qM0 WM0 1qM0 WM0 1qM0 11A0 1o00 11A0 1o00 11A0 1qM0 WM0 1qM0 WM0 1qM0 11A0 1o00 11A0 1o00 11A0 1o00 11A0 1qM0 WM0 1qM0 WM0 1qM0 11A0 1o00 11A0 1o00 11A0 1qM0 WM0 1qM0 WM0 1qM0 11A0 1o00 11A0 1o00 11A0 1o00 11A0 1qM0 WM0 1qM0 WM0 1qM0 11A0 1o00 11A0 1o00 11A0 1qM0 WM0 1qM0 WM0 1qM0 WM0 1qM0 11A0 1o00 11A0 1o00|62e5');
var _elm_community$elm_time$Time_TimeZoneData$europe_malta_l = _elm_community$elm_time$Time_TimeZoneData$unpack('Europe/Malta|CET CEST|-10 -20|0101010101010101010101010101010101010101010101010101010101010101010101010101010101010101010101010101010101010101010101010101010101010101010101010101010101010101010101010|-2arB0 Lz0 1cN0 1db0 1410 1on0 Wp0 1qL0 17d0 1cL0 M3B0 5M20 WM0 1fA0 1co0 17c0 1iM0 16m0 1de0 1lc0 14m0 1lc0 WO0 1qM0 GTW0 On0 1C10 LA0 1C00 LA0 1EM0 LA0 1C00 LA0 1zc0 Oo0 1C00 Oo0 1co0 1cM0 1lA0 Xc0 1qq0 11z0 1o10 11z0 1o10 11z0 1o10 11z0 1o10 11z0 1iN0 19z0 1fB0 1cM0 1cM0 1cM0 1cM0 1cM0 1cM0 1fA0 1cM0 1cM0 1cM0 1cM0 1cM0 1cM0 1cM0 1cM0 1cM0 1cM0 1cM0 1fA0 1cM0 1cM0 1cM0 1cM0 1cM0 1cM0 1cM0 1cM0 1cM0 1cM0 1fA0 1o00 11A0 1o00 11A0 1o00 11A0 1qM0 WM0 1qM0 WM0 1qM0 11A0 1o00 11A0 1o00 11A0 1qM0 WM0 1qM0 WM0 1qM0 WM0 1qM0 11A0 1o00 11A0 1o00 11A0 1qM0 WM0 1qM0 WM0 1qM0 11A0 1o00 11A0 1o00 11A0 1o00 11A0 1qM0 WM0 1qM0 WM0 1qM0 11A0 1o00 11A0 1o00 11A0 1qM0 WM0 1qM0 WM0 1qM0 11A0 1o00 11A0 1o00 11A0 1o00 11A0 1qM0 WM0 1qM0 WM0 1qM0 11A0 1o00 11A0 1o00 11A0 1qM0 WM0 1qM0 WM0 1qM0 WM0 1qM0 11A0 1o00 11A0 1o00|42e4');
var _elm_community$elm_time$Time_TimeZoneData$europe_minsk_l = _elm_community$elm_time$Time_TimeZoneData$unpack('Europe/Minsk|MMT EET MSK CEST CET MSD EEST +03|-1O -20 -30 -20 -10 -40 -30 -30|01234343252525252525252525261616161616161616161616161616161616161617|-1Pc1O eUnO qNX0 3gQ0 WM0 1fA0 1cM0 Al0 1tsn0 1db0 1cN0 1db0 1cN0 1db0 1dd0 1cO0 1cM0 1cM0 1cM0 1cM0 1cM0 1cM0 1cM0 1cM0 1cM0 1cM0 3Fc0 1cN0 1cM0 1cM0 1cM0 1cM0 1cM0 1cM0 1cM0 1cM0 1fA0 1o00 11A0 1o00 11A0 1o00 11A0 1qM0 WM0 1qM0 WM0 1qM0 11A0 1o00 11A0 1o00 11A0 1qM0 WM0 1qM0 WM0 1qM0 WM0 1qM0 11A0 1o00 11A0 1o00 11A0 1qM0 WM0|19e5');
var _elm_community$elm_time$Time_TimeZoneData$europe_monaco_l = _elm_community$elm_time$Time_TimeZoneData$unpack('Europe/Monaco|PMT WET WEST WEMT CET CEST|-9.l 0 -10 -20 -10 -20|01212121212121212121212121212121212121212121212121232323232345454545454545454545454545454545454545454545454545454545454545454545454545454545454545454545454545454545454545454545454545454|-2nco9.l cNb9.l HA0 19A0 1iM0 11c0 1oo0 Wo0 1rc0 QM0 1EM0 UM0 1u00 10o0 1io0 1wo0 Rc0 1a00 1fA0 1cM0 1cM0 1io0 17c0 1fA0 1a00 1io0 1a00 1io0 17c0 1fA0 1a00 1io0 17c0 1cM0 1cM0 1a00 1io0 1cM0 1cM0 1a00 1fA0 1io0 17c0 1cM0 1cM0 1a00 1fA0 1io0 1qM0 Df0 2RV0 11z0 11B0 1ze0 WM0 1fA0 1cM0 1fa0 1aq0 16M0 1ekn0 1cL0 1fC0 1a00 1fA0 1cM0 1cM0 1cM0 1fA0 1a00 1cM0 1cM0 1cM0 1cM0 1cM0 1cM0 1cM0 1fA0 1cM0 1cM0 1cM0 1cM0 1cM0 1cM0 1cM0 1cM0 1cM0 1cM0 1cM0 1fA0 1cM0 1cM0 1cM0 1cM0 1cM0 1cM0 1cM0 1cM0 1cM0 1cM0 1fA0 1o00 11A0 1o00 11A0 1o00 11A0 1qM0 WM0 1qM0 WM0 1qM0 11A0 1o00 11A0 1o00 11A0 1qM0 WM0 1qM0 WM0 1qM0 WM0 1qM0 11A0 1o00 11A0 1o00 11A0 1qM0 WM0 1qM0 WM0 1qM0 11A0 1o00 11A0 1o00 11A0 1o00 11A0 1qM0 WM0 1qM0 WM0 1qM0 11A0 1o00 11A0 1o00 11A0 1qM0 WM0 1qM0 WM0 1qM0 11A0 1o00 11A0 1o00 11A0 1o00 11A0 1qM0 WM0 1qM0 WM0 1qM0 11A0 1o00 11A0 1o00 11A0 1qM0 WM0 1qM0 WM0 1qM0 WM0 1qM0 11A0 1o00 11A0 1o00|38e3');
var _elm_community$elm_time$Time_TimeZoneData$europe_moscow_l = _elm_community$elm_time$Time_TimeZoneData$unpack('Europe/Moscow|MMT MMT MST MDST MSD MSK +05 EET EEST MSK|-2u.h -2v.j -3v.j -4v.j -40 -30 -50 -20 -30 -40|012132345464575454545454545454545458754545454545454545454545454545454545454595|-2ag2u.h 2pyW.W 1bA0 11X0 GN0 1Hb0 c4v.j ik0 3DA0 dz0 15A0 c10 2q10 iM10 23CL0 1db0 1cN0 1db0 1cN0 1db0 1dd0 1cO0 1cM0 1cM0 1cM0 1cM0 1cM0 1cM0 1cM0 1cM0 1cM0 1cM0 1cM0 1fA0 1cM0 1cN0 IM0 rX0 1cM0 1cM0 1cM0 1cM0 1cM0 1cM0 1cM0 1fA0 1o00 11A0 1o00 11A0 1o00 11A0 1qM0 WM0 1qM0 WM0 1qM0 11A0 1o00 11A0 1o00 11A0 1qM0 WM0 1qM0 WM0 1qM0 WM0 1qM0 11A0 1o00 11A0 1o00 11A0 1qM0 WM0 8Hz0|16e6');
var _elm_community$elm_time$Time_TimeZoneData$europe_oslo_l = _elm_community$elm_time$Time_TimeZoneData$unpack('Europe/Oslo|CET CEST|-10 -20|010101010101010101010101010101010101010101010101010101010101010101010101010101010101010101010101010101010101010101010101010101010101010101010|-2awM0 Qm0 W6o0 5pf0 WM0 1fA0 1cM0 1cM0 1cM0 1cM0 wJc0 1fA0 1cM0 1cM0 1cM0 1cM0 1cM0 1cM0 1cM0 1cM0 1cM0 1fA0 1qM0 WM0 zpc0 1a00 1cM0 1cM0 1cM0 1cM0 1cM0 1cM0 1cM0 1fA0 1cM0 1cM0 1cM0 1cM0 1cM0 1cM0 1cM0 1cM0 1cM0 1cM0 1cM0 1fA0 1cM0 1cM0 1cM0 1cM0 1cM0 1cM0 1cM0 1cM0 1cM0 1cM0 1fA0 1o00 11A0 1o00 11A0 1o00 11A0 1qM0 WM0 1qM0 WM0 1qM0 11A0 1o00 11A0 1o00 11A0 1qM0 WM0 1qM0 WM0 1qM0 WM0 1qM0 11A0 1o00 11A0 1o00 11A0 1qM0 WM0 1qM0 WM0 1qM0 11A0 1o00 11A0 1o00 11A0 1o00 11A0 1qM0 WM0 1qM0 WM0 1qM0 11A0 1o00 11A0 1o00 11A0 1qM0 WM0 1qM0 WM0 1qM0 11A0 1o00 11A0 1o00 11A0 1o00 11A0 1qM0 WM0 1qM0 WM0 1qM0 11A0 1o00 11A0 1o00 11A0 1qM0 WM0 1qM0 WM0 1qM0 WM0 1qM0 11A0 1o00 11A0 1o00|62e4');
var _elm_community$elm_time$Time_TimeZoneData$europe_paris_l = _elm_community$elm_time$Time_TimeZoneData$unpack('Europe/Paris|PMT WET WEST CEST CET WEMT|-9.l 0 -10 -20 -10 -20|0121212121212121212121212121212121212121212121212123434352543434343434343434343434343434343434343434343434343434343434343434343434343434343434343434343434343434343434343434343434343434|-2nco8.l cNb8.l HA0 19A0 1iM0 11c0 1oo0 Wo0 1rc0 QM0 1EM0 UM0 1u00 10o0 1io0 1wo0 Rc0 1a00 1fA0 1cM0 1cM0 1io0 17c0 1fA0 1a00 1io0 1a00 1io0 17c0 1fA0 1a00 1io0 17c0 1cM0 1cM0 1a00 1io0 1cM0 1cM0 1a00 1fA0 1io0 17c0 1cM0 1cM0 1a00 1fA0 1io0 1qM0 Df0 Ik0 5M30 WM0 1fA0 1cM0 Vx0 hB0 1aq0 16M0 1ekn0 1cL0 1fC0 1a00 1fA0 1cM0 1cM0 1cM0 1fA0 1a00 1cM0 1cM0 1cM0 1cM0 1cM0 1cM0 1cM0 1fA0 1cM0 1cM0 1cM0 1cM0 1cM0 1cM0 1cM0 1cM0 1cM0 1cM0 1cM0 1fA0 1cM0 1cM0 1cM0 1cM0 1cM0 1cM0 1cM0 1cM0 1cM0 1cM0 1fA0 1o00 11A0 1o00 11A0 1o00 11A0 1qM0 WM0 1qM0 WM0 1qM0 11A0 1o00 11A0 1o00 11A0 1qM0 WM0 1qM0 WM0 1qM0 WM0 1qM0 11A0 1o00 11A0 1o00 11A0 1qM0 WM0 1qM0 WM0 1qM0 11A0 1o00 11A0 1o00 11A0 1o00 11A0 1qM0 WM0 1qM0 WM0 1qM0 11A0 1o00 11A0 1o00 11A0 1qM0 WM0 1qM0 WM0 1qM0 11A0 1o00 11A0 1o00 11A0 1o00 11A0 1qM0 WM0 1qM0 WM0 1qM0 11A0 1o00 11A0 1o00 11A0 1qM0 WM0 1qM0 WM0 1qM0 WM0 1qM0 11A0 1o00 11A0 1o00|11e6');
var _elm_community$elm_time$Time_TimeZoneData$europe_prague_l = _elm_community$elm_time$Time_TimeZoneData$unpack('Europe/Prague|CET CEST|-10 -20|010101010101010101010101010101010101010101010101010101010101010101010101010101010101010101010101010101010101010101010101010101010101010101010|-2aFe0 11d0 1iO0 11A0 1o00 11A0 Qrc0 6i00 WM0 1fA0 1cM0 16M0 1lc0 1tA0 17A0 11c0 1io0 17c0 1io0 17c0 1fc0 1ao0 1bNc0 1cM0 1fA0 1a00 1cM0 1cM0 1cM0 1cM0 1cM0 1cM0 1cM0 1fA0 1cM0 1cM0 1cM0 1cM0 1cM0 1cM0 1cM0 1cM0 1cM0 1cM0 1cM0 1fA0 1cM0 1cM0 1cM0 1cM0 1cM0 1cM0 1cM0 1cM0 1cM0 1cM0 1fA0 1o00 11A0 1o00 11A0 1o00 11A0 1qM0 WM0 1qM0 WM0 1qM0 11A0 1o00 11A0 1o00 11A0 1qM0 WM0 1qM0 WM0 1qM0 WM0 1qM0 11A0 1o00 11A0 1o00 11A0 1qM0 WM0 1qM0 WM0 1qM0 11A0 1o00 11A0 1o00 11A0 1o00 11A0 1qM0 WM0 1qM0 WM0 1qM0 11A0 1o00 11A0 1o00 11A0 1qM0 WM0 1qM0 WM0 1qM0 11A0 1o00 11A0 1o00 11A0 1o00 11A0 1qM0 WM0 1qM0 WM0 1qM0 11A0 1o00 11A0 1o00 11A0 1qM0 WM0 1qM0 WM0 1qM0 WM0 1qM0 11A0 1o00 11A0 1o00|13e5');
var _elm_community$elm_time$Time_TimeZoneData$europe_riga_l = _elm_community$elm_time$Time_TimeZoneData$unpack('Europe/Riga|RMT LST EET MSK CEST CET MSD EEST|-1A.y -2A.y -20 -30 -20 -10 -40 -30|010102345454536363636363636363727272727272727272727272727272727272727272727272727272727272727272727272727272727272727272727272|-25TzA.y 11A0 1iM0 ko0 gWm0 yDXA.y 2bX0 3fE0 WM0 1fA0 1cM0 1cM0 4m0 1sLy0 1db0 1cN0 1db0 1cN0 1db0 1dd0 1cO0 1cM0 1cM0 1cM0 1cM0 1cM0 1cM0 1cM0 1cM0 1cM0 1cN0 1cM0 1fA0 1cM0 1cM0 1cM0 1cM0 1cM0 1cM0 1cM0 1cM0 1cM0 1cM0 1fA0 1cM0 1cN0 1o00 11A0 1o00 11A0 1qM0 3oo0 1qM0 11A0 1o00 11A0 1o00 11A0 1qM0 WM0 1qM0 WM0 1qM0 WM0 1qM0 11A0 1o00 11A0 1o00 11A0 1qM0 WM0 1qM0 WM0 1qM0 11A0 1o00 11A0 1o00 11A0 1o00 11A0 1qM0 WM0 1qM0 WM0 1qM0 11A0 1o00 11A0 1o00 11A0 1qM0 WM0 1qM0 WM0 1qM0 11A0 1o00 11A0 1o00 11A0 1o00 11A0 1qM0 WM0 1qM0 WM0 1qM0 11A0 1o00 11A0 1o00 11A0 1qM0 WM0 1qM0 WM0 1qM0 WM0 1qM0 11A0 1o00 11A0 1o00|64e4');
var _elm_community$elm_time$Time_TimeZoneData$europe_rome_l = _elm_community$elm_time$Time_TimeZoneData$unpack('Europe/Rome|CET CEST|-10 -20|0101010101010101010101010101010101010101010101010101010101010101010101010101010101010101010101010101010101010101010101010101010101010101010101010101010101010101010101010|-2arB0 Lz0 1cN0 1db0 1410 1on0 Wp0 1qL0 17d0 1cL0 M3B0 5M20 WM0 1fA0 1cM0 16M0 1iM0 16m0 1de0 1lc0 14m0 1lc0 WO0 1qM0 GTW0 On0 1C10 LA0 1C00 LA0 1EM0 LA0 1C00 LA0 1zc0 Oo0 1C00 Oo0 1C00 LA0 1zc0 Oo0 1C00 LA0 1C00 LA0 1zc0 Oo0 1C00 Oo0 1zc0 Oo0 1fC0 1a00 1cM0 1cM0 1cM0 1cM0 1cM0 1cM0 1cM0 1fA0 1cM0 1cM0 1cM0 1cM0 1cM0 1cM0 1cM0 1cM0 1cM0 1cM0 1cM0 1fA0 1cM0 1cM0 1cM0 1cM0 1cM0 1cM0 1cM0 1cM0 1cM0 1cM0 1fA0 1o00 11A0 1o00 11A0 1o00 11A0 1qM0 WM0 1qM0 WM0 1qM0 11A0 1o00 11A0 1o00 11A0 1qM0 WM0 1qM0 WM0 1qM0 WM0 1qM0 11A0 1o00 11A0 1o00 11A0 1qM0 WM0 1qM0 WM0 1qM0 11A0 1o00 11A0 1o00 11A0 1o00 11A0 1qM0 WM0 1qM0 WM0 1qM0 11A0 1o00 11A0 1o00 11A0 1qM0 WM0 1qM0 WM0 1qM0 11A0 1o00 11A0 1o00 11A0 1o00 11A0 1qM0 WM0 1qM0 WM0 1qM0 11A0 1o00 11A0 1o00 11A0 1qM0 WM0 1qM0 WM0 1qM0 WM0 1qM0 11A0 1o00 11A0 1o00|39e5');
var _elm_community$elm_time$Time_TimeZoneData$europe_samara_l = _elm_community$elm_time$Time_TimeZoneData$unpack('Europe/Samara|LMT +03 +04 +05|-3k.k -30 -40 -50|0123232323232323232121232323232323232323232323232323232323212|-22WM0 qH90 23CL0 1db0 1cN0 1db0 1cN0 1db0 1dd0 1cO0 1cM0 1cM0 1cM0 1cM0 1cM0 1cM0 1cM0 1cM0 2pB0 1cM0 1fA0 2y10 14m0 1cM0 1cM0 1cM0 1cM0 1cM0 1cM0 1cM0 1fA0 1o00 11A0 1o00 11A0 1o00 11A0 1qM0 WM0 1qM0 WM0 1qM0 11A0 1o00 11A0 1o00 11A0 1qM0 WM0 1qM0 WM0 1qM0 WM0 1qM0 11A0 1o00 11A0 1o00 2sp0 WM0|12e5');
var _elm_community$elm_time$Time_TimeZoneData$europe_saratov_l = _elm_community$elm_time$Time_TimeZoneData$unpack('Europe/Saratov|LMT +03 +04 +05|-34.i -30 -40 -50|012323232323232321212121212121212121212121212121212121212121212|-22WM0 qH90 23CL0 1db0 1cN0 1db0 1cN0 1db0 1dd0 1cO0 1cM0 1cM0 1cM0 1cM0 1cM0 1cM0 2pB0 1cM0 1cM0 1cM0 1fA0 1cM0 3Co0 1cM0 1cM0 1cM0 1cM0 1cM0 1cM0 1fA0 1o00 11A0 1o00 11A0 1o00 11A0 1qM0 WM0 1qM0 WM0 1qM0 11A0 1o00 11A0 1o00 11A0 1qM0 WM0 1qM0 WM0 1qM0 WM0 1qM0 11A0 1o00 11A0 1o00 11A0 1qM0 WM0 8Hz0 5810');
var _elm_community$elm_time$Time_TimeZoneData$europe_simferopol_l = _elm_community$elm_time$Time_TimeZoneData$unpack('Europe/Simferopol|SMT EET MSK CEST CET MSD EEST MSK|-2g -20 -30 -20 -10 -40 -30 -40|012343432525252525252525252161616525252616161616161616161616161616161616172|-1Pc2g eUog rEn0 2qs0 WM0 1fA0 1cM0 3V0 1u0L0 1db0 1cN0 1db0 1cN0 1db0 1dd0 1cO0 1cM0 1cM0 1cM0 1cM0 1cM0 1cM0 1cM0 1cM0 1cM0 1cM0 1Q00 4eL0 1cL0 1cN0 1cL0 1cN0 dX0 WL0 1cN0 1cL0 1fB0 1o30 11B0 1o00 11A0 1o00 11A0 1qM0 WM0 1qM0 WM0 1qM0 11A0 1o00 11A0 1o00 11A0 1qM0 WM0 1qM0 WM0 1qM0 WM0 1qM0 11A0 1o00 11A0 1o00 11A0 1qM0 WM0 1qM0 WM0 1qM0 11A0 1o00 11z0 1nW0|33e4');
var _elm_community$elm_time$Time_TimeZoneData$europe_sofia_l = _elm_community$elm_time$Time_TimeZoneData$unpack('Europe/Sofia|EET CET CEST EEST|-20 -10 -20 -30|01212103030303030303030303030303030303030303030303030303030303030303030303030303030303030303030303030303030303030303030303030|-168L0 WM0 1fA0 1cM0 1cM0 1cN0 1mKH0 1dd0 1fb0 1ap0 1fb0 1a20 1fy0 1a30 1cM0 1cM0 1cM0 1fA0 1cM0 1cM0 1cM0 1cM0 1cM0 1cM0 1cM0 1cM0 1cM0 1cM0 1cM0 1fA0 1cK0 1cL0 1cN0 1cL0 1cN0 1cL0 1cN0 1cL0 1cN0 1cL0 1fB0 1nX0 11E0 1o00 11A0 1o00 11A0 1qM0 WM0 1qM0 WM0 1qM0 11A0 1o00 11A0 1o00 11A0 1qM0 WM0 1qM0 WM0 1qM0 WM0 1qM0 11A0 1o00 11A0 1o00 11A0 1qM0 WM0 1qM0 WM0 1qM0 11A0 1o00 11A0 1o00 11A0 1o00 11A0 1qM0 WM0 1qM0 WM0 1qM0 11A0 1o00 11A0 1o00 11A0 1qM0 WM0 1qM0 WM0 1qM0 11A0 1o00 11A0 1o00 11A0 1o00 11A0 1qM0 WM0 1qM0 WM0 1qM0 11A0 1o00 11A0 1o00 11A0 1qM0 WM0 1qM0 WM0 1qM0 WM0 1qM0 11A0 1o00 11A0 1o00|12e5');
var _elm_community$elm_time$Time_TimeZoneData$europe_stockholm_l = _elm_community$elm_time$Time_TimeZoneData$unpack('Europe/Stockholm|CET CEST|-10 -20|01010101010101010101010101010101010101010101010101010101010101010101010101010101010101010101010101010101010101010101010|-2azC0 TB0 2yDe0 1a00 1cM0 1cM0 1cM0 1cM0 1cM0 1cM0 1cM0 1fA0 1cM0 1cM0 1cM0 1cM0 1cM0 1cM0 1cM0 1cM0 1cM0 1cM0 1cM0 1fA0 1cM0 1cM0 1cM0 1cM0 1cM0 1cM0 1cM0 1cM0 1cM0 1cM0 1fA0 1o00 11A0 1o00 11A0 1o00 11A0 1qM0 WM0 1qM0 WM0 1qM0 11A0 1o00 11A0 1o00 11A0 1qM0 WM0 1qM0 WM0 1qM0 WM0 1qM0 11A0 1o00 11A0 1o00 11A0 1qM0 WM0 1qM0 WM0 1qM0 11A0 1o00 11A0 1o00 11A0 1o00 11A0 1qM0 WM0 1qM0 WM0 1qM0 11A0 1o00 11A0 1o00 11A0 1qM0 WM0 1qM0 WM0 1qM0 11A0 1o00 11A0 1o00 11A0 1o00 11A0 1qM0 WM0 1qM0 WM0 1qM0 11A0 1o00 11A0 1o00 11A0 1qM0 WM0 1qM0 WM0 1qM0 WM0 1qM0 11A0 1o00 11A0 1o00|15e5');
var _elm_community$elm_time$Time_TimeZoneData$europe_tallinn_l = _elm_community$elm_time$Time_TimeZoneData$unpack('Europe/Tallinn|TMT CET CEST EET MSK MSD EEST|-1D -10 -20 -20 -30 -40 -30|012103421212454545454545454546363636363636363636363636363636363636363636363636363636363636363636363636363636363636363636363|-26oND teD 11A0 1Ta0 4rXl KSLD 2FX0 2Jg0 WM0 1fA0 1cM0 18J0 1sTX0 1db0 1cN0 1db0 1cN0 1db0 1dd0 1cO0 1cM0 1cM0 1cM0 1cM0 1cM0 1cM0 1cM0 1cM0 1cM0 1cN0 1cM0 1fA0 1cM0 1cM0 1cM0 1cM0 1cM0 1cM0 1cM0 1cM0 1cM0 1cM0 1fA0 1o00 11A0 1o00 11A0 1o10 11A0 1qM0 5QM0 1o00 11A0 1o00 11A0 1qM0 WM0 1qM0 WM0 1qM0 WM0 1qM0 11A0 1o00 11A0 1o00 11A0 1qM0 WM0 1qM0 WM0 1qM0 11A0 1o00 11A0 1o00 11A0 1o00 11A0 1qM0 WM0 1qM0 WM0 1qM0 11A0 1o00 11A0 1o00 11A0 1qM0 WM0 1qM0 WM0 1qM0 11A0 1o00 11A0 1o00 11A0 1o00 11A0 1qM0 WM0 1qM0 WM0 1qM0 11A0 1o00 11A0 1o00 11A0 1qM0 WM0 1qM0 WM0 1qM0 WM0 1qM0 11A0 1o00 11A0 1o00|41e4');
var _elm_community$elm_time$Time_TimeZoneData$europe_tirane_l = _elm_community$elm_time$Time_TimeZoneData$unpack('Europe/Tirane|LMT CET CEST|-1j.k -10 -20|01212121212121212121212121212121212121212121212121212121212121212121212121212121212121212121212121212121212121212121212121212121212121|-2glBj.k 14pcj.k 5LC0 WM0 4M0 1fCK0 10n0 1op0 11z0 1pd0 11z0 1qN0 WL0 1qp0 Xb0 1qp0 Xb0 1qp0 11z0 1lB0 11z0 1qN0 11z0 1iN0 16n0 1dd0 1cO0 1cM0 1cM0 1cM0 1cM0 1cM0 1cM0 1cM0 1cM0 1cM0 1cM0 1cM0 1fA0 1cM0 1cM0 1cM0 1cM0 1cM0 1cM0 1cM0 1cM0 1cM0 1cM0 1fA0 1o00 11A0 1o00 11A0 1o00 11A0 1qM0 WM0 1qM0 WM0 1qM0 11A0 1o00 11A0 1o00 11A0 1qM0 WM0 1qM0 WM0 1qM0 WM0 1qM0 11A0 1o00 11A0 1o00 11A0 1qM0 WM0 1qM0 WM0 1qM0 11A0 1o00 11A0 1o00 11A0 1o00 11A0 1qM0 WM0 1qM0 WM0 1qM0 11A0 1o00 11A0 1o00 11A0 1qM0 WM0 1qM0 WM0 1qM0 11A0 1o00 11A0 1o00 11A0 1o00 11A0 1qM0 WM0 1qM0 WM0 1qM0 11A0 1o00 11A0 1o00 11A0 1qM0 WM0 1qM0 WM0 1qM0 WM0 1qM0 11A0 1o00 11A0 1o00|42e4');
var _elm_community$elm_time$Time_TimeZoneData$europe_ulyanovsk_l = _elm_community$elm_time$Time_TimeZoneData$unpack('Europe/Ulyanovsk|LMT +03 +04 +05 +02|-3d.A -30 -40 -50 -20|01232323232323232321214121212121212121212121212121212121212121212|-22WM0 qH90 23CL0 1db0 1cN0 1db0 1cN0 1db0 1dd0 1cO0 1cM0 1cM0 1cM0 1cM0 1cM0 1cM0 1cM0 1cM0 2pB0 1cM0 1fA0 2pB0 IM0 rX0 1cM0 1cM0 1cM0 1cM0 1cM0 1cM0 1cM0 1fA0 1o00 11A0 1o00 11A0 1o00 11A0 1qM0 WM0 1qM0 WM0 1qM0 11A0 1o00 11A0 1o00 11A0 1qM0 WM0 1qM0 WM0 1qM0 WM0 1qM0 11A0 1o00 11A0 1o00 11A0 1qM0 WM0 8Hz0 3rd0');
var _elm_community$elm_time$Time_TimeZoneData$europe_uzhgorod_l = _elm_community$elm_time$Time_TimeZoneData$unpack('Europe/Uzhgorod|CET CEST MSK MSD EET EEST|-10 -20 -30 -40 -20 -30|010101023232323232323232320454545454545454545454545454545454545454545454545454545454545454545454545454545454545454545454|-1cqL0 6i00 WM0 1fA0 1cM0 1ml0 1Cp0 1r3W0 1db0 1cN0 1db0 1cN0 1db0 1dd0 1cO0 1cM0 1cM0 1cM0 1cM0 1cM0 1cM0 1cM0 1cM0 1cM0 1cM0 1Q00 1Nf0 2pw0 1cL0 1cN0 1cL0 1cN0 1cL0 1cQ0 1cM0 1fA0 1o00 11A0 1o00 11A0 1o00 11A0 1qM0 WM0 1qM0 WM0 1qM0 11A0 1o00 11A0 1o00 11A0 1qM0 WM0 1qM0 WM0 1qM0 WM0 1qM0 11A0 1o00 11A0 1o00 11A0 1qM0 WM0 1qM0 WM0 1qM0 11A0 1o00 11A0 1o00 11A0 1o00 11A0 1qM0 WM0 1qM0 WM0 1qM0 11A0 1o00 11A0 1o00 11A0 1qM0 WM0 1qM0 WM0 1qM0 11A0 1o00 11A0 1o00 11A0 1o00 11A0 1qM0 WM0 1qM0 WM0 1qM0 11A0 1o00 11A0 1o00 11A0 1qM0 WM0 1qM0 WM0 1qM0 WM0 1qM0 11A0 1o00 11A0 1o00|11e4');
var _elm_community$elm_time$Time_TimeZoneData$europe_vienna_l = _elm_community$elm_time$Time_TimeZoneData$unpack('Europe/Vienna|CET CEST|-10 -20|0101010101010101010101010101010101010101010101010101010101010101010101010101010101010101010101010101010101010101010101010101010101010101010|-2aFe0 11d0 1iO0 11A0 1o00 11A0 3KM0 14o0 LA00 6i00 WM0 1fA0 1cM0 1cM0 1cM0 400 2qM0 1a00 1cM0 1cM0 1io0 17c0 1gHa0 19X0 1cP0 1cM0 1cM0 1cM0 1cM0 1cM0 1cM0 1fA0 1cM0 1cM0 1cM0 1cM0 1cM0 1cM0 1cM0 1cM0 1cM0 1cM0 1cM0 1fA0 1cM0 1cM0 1cM0 1cM0 1cM0 1cM0 1cM0 1cM0 1cM0 1cM0 1fA0 1o00 11A0 1o00 11A0 1o00 11A0 1qM0 WM0 1qM0 WM0 1qM0 11A0 1o00 11A0 1o00 11A0 1qM0 WM0 1qM0 WM0 1qM0 WM0 1qM0 11A0 1o00 11A0 1o00 11A0 1qM0 WM0 1qM0 WM0 1qM0 11A0 1o00 11A0 1o00 11A0 1o00 11A0 1qM0 WM0 1qM0 WM0 1qM0 11A0 1o00 11A0 1o00 11A0 1qM0 WM0 1qM0 WM0 1qM0 11A0 1o00 11A0 1o00 11A0 1o00 11A0 1qM0 WM0 1qM0 WM0 1qM0 11A0 1o00 11A0 1o00 11A0 1qM0 WM0 1qM0 WM0 1qM0 WM0 1qM0 11A0 1o00 11A0 1o00|18e5');
var _elm_community$elm_time$Time_TimeZoneData$europe_vilnius_l = _elm_community$elm_time$Time_TimeZoneData$unpack('Europe/Vilnius|WMT KMT CET EET MSK CEST MSD EEST|-1o -1z.A -10 -20 -30 -20 -40 -30|012324525254646464646464646473737373737373737352537373737373737373737373737373737373737373737373737373737373737373737373|-293do 6ILM.o 1Ooz.A zz0 Mfd0 29W0 3is0 WM0 1fA0 1cM0 LV0 1tgL0 1db0 1cN0 1db0 1cN0 1db0 1dd0 1cO0 1cM0 1cM0 1cM0 1cM0 1cM0 1cM0 1cM0 1cM0 1cM0 1cN0 1cM0 1fA0 1cM0 1cM0 1cM0 1cM0 1cM0 1cM0 1cM0 1cM0 1cM0 1cM0 1fA0 1o00 11A0 1o00 11B0 1o00 11A0 1qM0 8io0 1o00 11A0 1qM0 WM0 1qM0 WM0 1qM0 WM0 1qM0 11A0 1o00 11A0 1o00 11A0 1qM0 WM0 1qM0 WM0 1qM0 11A0 1o00 11A0 1o00 11A0 1o00 11A0 1qM0 WM0 1qM0 WM0 1qM0 11A0 1o00 11A0 1o00 11A0 1qM0 WM0 1qM0 WM0 1qM0 11A0 1o00 11A0 1o00 11A0 1o00 11A0 1qM0 WM0 1qM0 WM0 1qM0 11A0 1o00 11A0 1o00 11A0 1qM0 WM0 1qM0 WM0 1qM0 WM0 1qM0 11A0 1o00 11A0 1o00|54e4');
var _elm_community$elm_time$Time_TimeZoneData$europe_volgograd_l = _elm_community$elm_time$Time_TimeZoneData$unpack('Europe/Volgograd|LMT +03 +04 +05|-2V.E -30 -40 -50|01232323232323232121212121212121212121212121212121212121212121|-21IqV.E psLV.E 23CL0 1db0 1cN0 1db0 1cN0 1db0 1dd0 1cO0 1cM0 1cM0 1cM0 1cM0 1cM0 1cM0 2pB0 1cM0 1cM0 1cM0 1fA0 1cM0 3Co0 1cM0 1cM0 1cM0 1cM0 1cM0 1cM0 1fA0 1o00 11A0 1o00 11A0 1o00 11A0 1qM0 WM0 1qM0 WM0 1qM0 11A0 1o00 11A0 1o00 11A0 1qM0 WM0 1qM0 WM0 1qM0 WM0 1qM0 11A0 1o00 11A0 1o00 11A0 1qM0 WM0 8Hz0|10e5');
var _elm_community$elm_time$Time_TimeZoneData$europe_warsaw_l = _elm_community$elm_time$Time_TimeZoneData$unpack('Europe/Warsaw|WMT CET CEST EET EEST|-1o -10 -20 -20 -30|012121234312121212121212121212121212121212121212121212121212121212121212121212121212121212121212121212121212121212121212121212121212121212121212121212121212121212121|-2ctdo 1LXo 11d0 1iO0 11A0 1o00 11A0 1on0 11A0 6zy0 HWP0 5IM0 WM0 1fA0 1cM0 1dz0 1mL0 1en0 15B0 1aq0 1nA0 11A0 1io0 17c0 1fA0 1a00 iDX0 LA0 1cM0 1cM0 1C00 Oo0 1cM0 1cM0 1zc0 Oo0 1zc0 Oo0 1zc0 Oo0 1C00 LA0 uso0 1a00 1fA0 1cM0 1cM0 1cM0 1fA0 1a00 1cM0 1cM0 1cM0 1cM0 1cM0 1cM0 1cM0 1fA0 1cM0 1cM0 1cM0 1cM0 1cM0 1cM0 1cN0 1cM0 1cM0 1cM0 1cM0 1fA0 1cM0 1cM0 1cM0 1cM0 1cM0 1cM0 1cM0 1cM0 1cM0 1cM0 1fA0 1o00 11A0 1o00 11A0 1o00 11A0 1qM0 WM0 1qM0 WM0 1qM0 11A0 1o00 11A0 1o00 11A0 1qM0 WM0 1qM0 WM0 1qM0 WM0 1qM0 11A0 1o00 11A0 1o00 11A0 1qM0 WM0 1qM0 WM0 1qM0 11A0 1o00 11A0 1o00 11A0 1o00 11A0 1qM0 WM0 1qM0 WM0 1qM0 11A0 1o00 11A0 1o00 11A0 1qM0 WM0 1qM0 WM0 1qM0 11A0 1o00 11A0 1o00 11A0 1o00 11A0 1qM0 WM0 1qM0 WM0 1qM0 11A0 1o00 11A0 1o00 11A0 1qM0 WM0 1qM0 WM0 1qM0 WM0 1qM0 11A0 1o00 11A0 1o00|17e5');
var _elm_community$elm_time$Time_TimeZoneData$europe_zaporozhye_l = _elm_community$elm_time$Time_TimeZoneData$unpack('Europe/Zaporozhye|CUT EET MSK CEST CET MSD EEST|-2k -20 -30 -20 -10 -40 -30|01234342525252525252525252526161616161616161616161616161616161616161616161616161616161616161616161616161616161616161616161|-1Pc2k eUok rdb0 2RE0 WM0 1fA0 8m0 1v9a0 1db0 1cN0 1db0 1cN0 1db0 1dd0 1cO0 1cM0 1cM0 1cM0 1cM0 1cM0 1cM0 1cM0 1cM0 1cM0 1cM0 1cM0 1fA0 1cM0 1cK0 1cN0 1cL0 1cN0 1cL0 1cN0 1cL0 1cQ0 1cM0 1fA0 1o00 11A0 1o00 11A0 1o00 11A0 1qM0 WM0 1qM0 WM0 1qM0 11A0 1o00 11A0 1o00 11A0 1qM0 WM0 1qM0 WM0 1qM0 WM0 1qM0 11A0 1o00 11A0 1o00 11A0 1qM0 WM0 1qM0 WM0 1qM0 11A0 1o00 11A0 1o00 11A0 1o00 11A0 1qM0 WM0 1qM0 WM0 1qM0 11A0 1o00 11A0 1o00 11A0 1qM0 WM0 1qM0 WM0 1qM0 11A0 1o00 11A0 1o00 11A0 1o00 11A0 1qM0 WM0 1qM0 WM0 1qM0 11A0 1o00 11A0 1o00 11A0 1qM0 WM0 1qM0 WM0 1qM0 WM0 1qM0 11A0 1o00 11A0 1o00|77e4');
var _elm_community$elm_time$Time_TimeZoneData$europe_zurich_l = _elm_community$elm_time$Time_TimeZoneData$unpack('Europe/Zurich|CET CEST|-10 -20|01010101010101010101010101010101010101010101010101010101010101010101010101010101010101010101010101010101010101010101010|-19Lc0 11A0 1o00 11A0 1xG10 1cM0 1cM0 1cM0 1cM0 1cM0 1cM0 1fA0 1cM0 1cM0 1cM0 1cM0 1cM0 1cM0 1cM0 1cM0 1cM0 1cM0 1cM0 1fA0 1cM0 1cM0 1cM0 1cM0 1cM0 1cM0 1cM0 1cM0 1cM0 1cM0 1fA0 1o00 11A0 1o00 11A0 1o00 11A0 1qM0 WM0 1qM0 WM0 1qM0 11A0 1o00 11A0 1o00 11A0 1qM0 WM0 1qM0 WM0 1qM0 WM0 1qM0 11A0 1o00 11A0 1o00 11A0 1qM0 WM0 1qM0 WM0 1qM0 11A0 1o00 11A0 1o00 11A0 1o00 11A0 1qM0 WM0 1qM0 WM0 1qM0 11A0 1o00 11A0 1o00 11A0 1qM0 WM0 1qM0 WM0 1qM0 11A0 1o00 11A0 1o00 11A0 1o00 11A0 1qM0 WM0 1qM0 WM0 1qM0 11A0 1o00 11A0 1o00 11A0 1qM0 WM0 1qM0 WM0 1qM0 WM0 1qM0 11A0 1o00 11A0 1o00|38e4');
var _elm_community$elm_time$Time_TimeZoneData$hst_l = _elm_community$elm_time$Time_TimeZoneData$unpack('HST|HST|a0|0|');
var _elm_community$elm_time$Time_TimeZoneData$indian_chagos_l = _elm_community$elm_time$Time_TimeZoneData$unpack('Indian/Chagos|LMT IOT IOT|-4N.E -50 -60|012|-2xosN.E 3AGLN.E|30e2');
var _elm_community$elm_time$Time_TimeZoneData$indian_christmas_l = _elm_community$elm_time$Time_TimeZoneData$unpack('Indian/Christmas|CXT|-70|0||21e2');
var _elm_community$elm_time$Time_TimeZoneData$indian_cocos_l = _elm_community$elm_time$Time_TimeZoneData$unpack('Indian/Cocos|CCT|-6u|0||596');
var _elm_community$elm_time$Time_TimeZoneData$indian_kerguelen_l = _elm_community$elm_time$Time_TimeZoneData$unpack('Indian/Kerguelen|-00 +05|0 -50|01|-MG00|130');
var _elm_community$elm_time$Time_TimeZoneData$indian_mahe_l = _elm_community$elm_time$Time_TimeZoneData$unpack('Indian/Mahe|LMT SCT|-3F.M -40|01|-2yO3F.M|79e3');
var _elm_community$elm_time$Time_TimeZoneData$indian_maldives_l = _elm_community$elm_time$Time_TimeZoneData$unpack('Indian/Maldives|MMT MVT|-4S -50|01|-olgS|35e4');
var _elm_community$elm_time$Time_TimeZoneData$indian_mauritius_l = _elm_community$elm_time$Time_TimeZoneData$unpack('Indian/Mauritius|LMT MUT MUST|-3O -40 -50|012121|-2xorO 34unO 14L0 12kr0 11z0|15e4');
var _elm_community$elm_time$Time_TimeZoneData$indian_reunion_l = _elm_community$elm_time$Time_TimeZoneData$unpack('Indian/Reunion|LMT RET|-3F.Q -40|01|-2mDDF.Q|84e4');
var _elm_community$elm_time$Time_TimeZoneData$met_l = _elm_community$elm_time$Time_TimeZoneData$unpack('MET|MET MEST|-10 -20|01010101010101010101010101010101010101010101010101010101010101010101010101010101010101010101010101010101010101010101010101010101010101010|-2aFe0 11d0 1iO0 11A0 1o00 11A0 Qrc0 6i00 WM0 1fA0 1cM0 1cM0 1cM0 16M0 1gMM0 1a00 1fA0 1cM0 1cM0 1cM0 1fA0 1a00 1cM0 1cM0 1cM0 1cM0 1cM0 1cM0 1cM0 1fA0 1cM0 1cM0 1cM0 1cM0 1cM0 1cM0 1cM0 1cM0 1cM0 1cM0 1cM0 1fA0 1cM0 1cM0 1cM0 1cM0 1cM0 1cM0 1cM0 1cM0 1cM0 1cM0 1fA0 1o00 11A0 1o00 11A0 1o00 11A0 1qM0 WM0 1qM0 WM0 1qM0 11A0 1o00 11A0 1o00 11A0 1qM0 WM0 1qM0 WM0 1qM0 WM0 1qM0 11A0 1o00 11A0 1o00 11A0 1qM0 WM0 1qM0 WM0 1qM0 11A0 1o00 11A0 1o00 11A0 1o00 11A0 1qM0 WM0 1qM0 WM0 1qM0 11A0 1o00 11A0 1o00 11A0 1qM0 WM0 1qM0 WM0 1qM0 11A0 1o00 11A0 1o00 11A0 1o00 11A0 1qM0 WM0 1qM0 WM0 1qM0 11A0 1o00 11A0 1o00 11A0 1qM0 WM0 1qM0 WM0 1qM0 WM0 1qM0 11A0 1o00 11A0 1o00');
var _elm_community$elm_time$Time_TimeZoneData$mst7mdt_l = _elm_community$elm_time$Time_TimeZoneData$unpack('MST7MDT|MST MDT MWT MPT|70 60 60 60|010102301010101010101010101010101010101010101010101010101010101010101010101010101010101010101010101010101010101010101010101010101010101010101010101010|-261r0 1nX0 11B0 1nX0 SgN0 8x20 ix0 QwN0 1cL0 1cN0 1cL0 1cN0 1cL0 1cN0 1cL0 1cN0 1fz0 1cN0 1cL0 1cN0 1cL0 s10 1Vz0 LB0 1BX0 1cN0 1fz0 1a10 1fz0 1cN0 1cL0 1cN0 1cL0 1cN0 1cL0 1cN0 1cL0 1cN0 1fz0 1a10 1fz0 1cN0 1cL0 1cN0 1cL0 1cN0 1cL0 14p0 1lb0 14p0 1nX0 11B0 1nX0 11B0 1nX0 14p0 1lb0 14p0 1lb0 14p0 1nX0 11B0 1nX0 11B0 1nX0 14p0 1lb0 14p0 1lb0 14p0 1lb0 14p0 1nX0 11B0 1nX0 11B0 1nX0 14p0 1lb0 14p0 1lb0 14p0 1nX0 11B0 1nX0 11B0 1nX0 Rd0 1zb0 Op0 1zb0 Op0 1zb0 Rd0 1zb0 Op0 1zb0 Op0 1zb0 Op0 1zb0 Op0 1zb0 Op0 1zb0 Rd0 1zb0 Op0 1zb0 Op0 1zb0 Op0 1zb0 Op0 1zb0 Rd0 1zb0 Op0 1zb0 Op0 1zb0 Op0 1zb0 Op0 1zb0 Op0 1zb0 Rd0 1zb0 Op0 1zb0 Op0 1zb0 Op0 1zb0 Op0 1zb0 Rd0 1zb0 Op0 1zb0 Op0 1zb0 Op0 1zb0 Op0 1zb0 Op0 1zb0');
var _elm_community$elm_time$Time_TimeZoneData$mst_l = _elm_community$elm_time$Time_TimeZoneData$unpack('MST|MST|70|0|');
var _elm_community$elm_time$Time_TimeZoneData$pacific_apia_l = _elm_community$elm_time$Time_TimeZoneData$unpack('Pacific/Apia|LMT WSST SST SDT WSDT WSST|bq.U bu b0 a0 -e0 -d0|01232345454545454545454545454545454545454545454545454545454|-2nDMx.4 1yW03.4 2rRbu 1ff0 1a00 CI0 AQ0 1cM0 1fA0 1a00 1fA0 1a00 1fA0 1a00 1fA0 1a00 1fA0 1a00 1fA0 1cM0 1fA0 1a00 1fA0 1a00 1fA0 1a00 1fA0 1a00 1fA0 1a00 1io0 1a00 1fA0 1a00 1fA0 1a00 1fA0 1a00 1fA0 1a00 1fA0 1cM0 1fA0 1a00 1fA0 1a00 1fA0 1a00 1fA0 1a00 1fA0 1a00 1fA0 1cM0 1fA0 1a00 1fA0 1a00|37e3');
var _elm_community$elm_time$Time_TimeZoneData$pacific_auckland_l = _elm_community$elm_time$Time_TimeZoneData$unpack('Pacific/Auckland|NZMT NZST NZST NZDT|-bu -cu -c0 -d0|01020202020202020202020202023232323232323232323232323232323232323232323232323232323232323232323232323232323232323232323232323232323232323232323232323232323|-1GCVu Lz0 1tB0 11zu 1o0u 11zu 1o0u 11zu 1o0u 14nu 1lcu 14nu 1lcu 1lbu 11Au 1nXu 11Au 1nXu 11Au 1nXu 11Au 1nXu 11Au 1qLu WMu 1qLu 11Au 1n1bu IM0 1C00 Rc0 1zc0 Oo0 1zc0 Oo0 1zc0 Oo0 1zc0 Oo0 1zc0 Oo0 1zc0 Rc0 1zc0 Oo0 1zc0 Oo0 1zc0 Oo0 1zc0 Oo0 1zc0 Oo0 1zc0 Rc0 1zc0 Oo0 1qM0 14o0 1lc0 14o0 1lc0 14o0 1lc0 17c0 1io0 17c0 1io0 17c0 1io0 17c0 1lc0 14o0 1lc0 14o0 1lc0 17c0 1io0 17c0 1io0 17c0 1lc0 14o0 1lc0 14o0 1lc0 17c0 1io0 17c0 1io0 17c0 1io0 17c0 1io0 1fA0 1a00 1fA0 1a00 1fA0 1a00 1fA0 1a00 1fA0 1cM0 1fA0 1a00 1fA0 1a00 1fA0 1a00 1fA0 1a00 1fA0 1a00 1fA0 1cM0 1fA0 1a00 1fA0 1a00 1fA0 1a00 1fA0 1a00 1fA0 1a00 1io0 1a00 1fA0 1a00 1fA0 1a00 1fA0 1a00 1fA0 1a00 1fA0 1cM0 1fA0 1a00 1fA0 1a00 1fA0 1a00 1fA0 1a00 1fA0 1a00 1fA0 1cM0 1fA0 1a00 1fA0 1a00|14e5');
var _elm_community$elm_time$Time_TimeZoneData$pacific_bougainville_l = _elm_community$elm_time$Time_TimeZoneData$unpack('Pacific/Bougainville|PGT JST BST|-a0 -90 -b0|0102|-16Wy0 7CN0 2MQp0|18e4');
var _elm_community$elm_time$Time_TimeZoneData$pacific_chatham_l = _elm_community$elm_time$Time_TimeZoneData$unpack('Pacific/Chatham|CHAST CHAST CHADT|-cf -cJ -dJ|012121212121212121212121212121212121212121212121212121212121212121212121212121212121212121212121212121212121212121212121212121212|-WqAf 1adef IM0 1C00 Rc0 1zc0 Oo0 1zc0 Oo0 1zc0 Oo0 1zc0 Oo0 1zc0 Oo0 1zc0 Rc0 1zc0 Oo0 1zc0 Oo0 1zc0 Oo0 1zc0 Oo0 1zc0 Oo0 1zc0 Rc0 1zc0 Oo0 1qM0 14o0 1lc0 14o0 1lc0 14o0 1lc0 17c0 1io0 17c0 1io0 17c0 1io0 17c0 1lc0 14o0 1lc0 14o0 1lc0 17c0 1io0 17c0 1io0 17c0 1lc0 14o0 1lc0 14o0 1lc0 17c0 1io0 17c0 1io0 17c0 1io0 17c0 1io0 1fA0 1a00 1fA0 1a00 1fA0 1a00 1fA0 1a00 1fA0 1cM0 1fA0 1a00 1fA0 1a00 1fA0 1a00 1fA0 1a00 1fA0 1a00 1fA0 1cM0 1fA0 1a00 1fA0 1a00 1fA0 1a00 1fA0 1a00 1fA0 1a00 1io0 1a00 1fA0 1a00 1fA0 1a00 1fA0 1a00 1fA0 1a00 1fA0 1cM0 1fA0 1a00 1fA0 1a00 1fA0 1a00 1fA0 1a00 1fA0 1a00 1fA0 1cM0 1fA0 1a00 1fA0 1a00|600');
var _elm_community$elm_time$Time_TimeZoneData$pacific_chuuk_l = _elm_community$elm_time$Time_TimeZoneData$unpack('Pacific/Chuuk|CHUT|-a0|0||49e3');
var _elm_community$elm_time$Time_TimeZoneData$pacific_easter_l = _elm_community$elm_time$Time_TimeZoneData$unpack('Pacific/Easter|EMT EAST EASST EAST EASST|7h.s 70 60 60 50|0121212121212121212121212121234343434343434343434343434343434343434343434343434343434343434343434343434343434343434343434343434343434343434|-1uSgG.w 1s4IG.w WL0 1zd0 On0 1ip0 11z0 1o10 11z0 1qN0 WL0 1ld0 14n0 1qN0 WL0 1qN0 11z0 1o10 11z0 1o10 11z0 1qN0 WL0 1qN0 WL0 1qN0 11z0 1o10 11z0 1o10 11z0 1o10 11z0 1qN0 WL0 1qN0 WL0 1qN0 1cL0 1cN0 11z0 1o10 11z0 1qN0 WL0 1fB0 19X0 1qN0 11z0 1o10 11z0 1o10 11z0 1o10 11z0 1qN0 WL0 1qN0 17b0 1ip0 11z0 1ip0 1fz0 1fB0 11z0 1qN0 WL0 1qN0 WL0 1qN0 WL0 1qN0 11z0 1o10 11z0 1o10 11z0 1qN0 WL0 1qN0 17b0 1ip0 11z0 1o10 19X0 1fB0 1nX0 G10 1EL0 Op0 1zb0 Rd0 1wn0 Rd0 46n0 Ap0 1Nb0 Ap0 1Nb0 Ap0 1Nb0 Ap0 1Nb0 Ap0 1Nb0 Dd0 1Nb0 Ap0 1Nb0 Ap0 1Nb0 Ap0 1Nb0 Ap0 1Nb0 Ap0 1Nb0 Dd0 1Nb0 Ap0 1Nb0 Ap0 1Nb0 Ap0 1Nb0 Ap0 1Nb0 Dd0 1Nb0 Ap0 1Nb0 Ap0 1Nb0 Ap0 1Nb0 Ap0 1Nb0 Ap0|30e2');
var _elm_community$elm_time$Time_TimeZoneData$pacific_efate_l = _elm_community$elm_time$Time_TimeZoneData$unpack('Pacific/Efate|LMT VUT VUST|-bd.g -b0 -c0|0121212121212121212121|-2l9nd.g 2Szcd.g 1cL0 1oN0 10L0 1fB0 19X0 1fB0 1cL0 1cN0 1cL0 1cN0 1cL0 1cN0 1cL0 1cN0 1cL0 1fB0 Lz0 1Nd0 An0|66e3');
var _elm_community$elm_time$Time_TimeZoneData$pacific_enderbury_l = _elm_community$elm_time$Time_TimeZoneData$unpack('Pacific/Enderbury|PHOT PHOT PHOT|c0 b0 -d0|012|nIc0 B8n0|1');
var _elm_community$elm_time$Time_TimeZoneData$pacific_fakaofo_l = _elm_community$elm_time$Time_TimeZoneData$unpack('Pacific/Fakaofo|TKT TKT|b0 -d0|01|1Gfn0|483');
var _elm_community$elm_time$Time_TimeZoneData$pacific_fiji_l = _elm_community$elm_time$Time_TimeZoneData$unpack('Pacific/Fiji|LMT FJT FJST|-bT.I -c0 -d0|0121212121212121212121212121212121212121212121212121212121212121|-2bUzT.I 3m8NT.I LA0 1EM0 IM0 nJc0 LA0 1o00 Rc0 1wo0 Ao0 1Nc0 Ao0 1Q00 xz0 1SN0 uM0 1SM0 uM0 1VA0 s00 1VA0 uM0 1SM0 uM0 1SM0 uM0 1SM0 uM0 1VA0 s00 1VA0 s00 1VA0 uM0 1SM0 uM0 1SM0 uM0 1SM0 uM0 1VA0 s00 1VA0 uM0 1SM0 uM0 1SM0 uM0 1SM0 uM0 1VA0 s00 1VA0 s00 1VA0 uM0 1SM0 uM0 1SM0 uM0 1SM0 uM0|88e4');
var _elm_community$elm_time$Time_TimeZoneData$pacific_funafuti_l = _elm_community$elm_time$Time_TimeZoneData$unpack('Pacific/Funafuti|TVT|-c0|0||45e2');
var _elm_community$elm_time$Time_TimeZoneData$pacific_galapagos_l = _elm_community$elm_time$Time_TimeZoneData$unpack('Pacific/Galapagos|LMT ECT GALT|5W.o 50 60|012|-1yVS1.A 2dTz1.A|25e3');
var _elm_community$elm_time$Time_TimeZoneData$pacific_gambier_l = _elm_community$elm_time$Time_TimeZoneData$unpack('Pacific/Gambier|LMT GAMT|8X.M 90|01|-2jof0.c|125');
var _elm_community$elm_time$Time_TimeZoneData$pacific_guadalcanal_l = _elm_community$elm_time$Time_TimeZoneData$unpack('Pacific/Guadalcanal|LMT SBT|-aD.M -b0|01|-2joyD.M|11e4');
var _elm_community$elm_time$Time_TimeZoneData$pacific_guam_l = _elm_community$elm_time$Time_TimeZoneData$unpack('Pacific/Guam|GST ChST|-a0 -a0|01|1fpq0|17e4');
var _elm_community$elm_time$Time_TimeZoneData$pacific_honolulu_l = _elm_community$elm_time$Time_TimeZoneData$unpack('Pacific/Honolulu|HST HDT HST|au 9u a0|010102|-1thLu 8x0 lef0 8Pz0 46p0|37e4');
var _elm_community$elm_time$Time_TimeZoneData$pacific_kiritimati_l = _elm_community$elm_time$Time_TimeZoneData$unpack('Pacific/Kiritimati|LINT LINT LINT|aE a0 -e0|012|nIaE B8nk|51e2');
var _elm_community$elm_time$Time_TimeZoneData$pacific_kosrae_l = _elm_community$elm_time$Time_TimeZoneData$unpack('Pacific/Kosrae|KOST KOST|-b0 -c0|010|-AX0 1bdz0|66e2');
var _elm_community$elm_time$Time_TimeZoneData$pacific_kwajalein_l = _elm_community$elm_time$Time_TimeZoneData$unpack('Pacific/Kwajalein|MHT KWAT MHT|-b0 c0 -c0|012|-AX0 W9X0|14e3');
var _elm_community$elm_time$Time_TimeZoneData$pacific_majuro_l = _elm_community$elm_time$Time_TimeZoneData$unpack('Pacific/Majuro|MHT MHT|-b0 -c0|01|-AX0|28e3');
var _elm_community$elm_time$Time_TimeZoneData$pacific_marquesas_l = _elm_community$elm_time$Time_TimeZoneData$unpack('Pacific/Marquesas|LMT MART|9i 9u|01|-2joeG|86e2');
var _elm_community$elm_time$Time_TimeZoneData$pacific_nauru_l = _elm_community$elm_time$Time_TimeZoneData$unpack('Pacific/Nauru|LMT NRT JST NRT|-b7.E -bu -90 -c0|01213|-1Xdn7.E PvzB.E 5RCu 1ouJu|10e3');
var _elm_community$elm_time$Time_TimeZoneData$pacific_niue_l = _elm_community$elm_time$Time_TimeZoneData$unpack('Pacific/Niue|NUT NUT NUT|bk bu b0|012|-KfME 17y0a|12e2');
var _elm_community$elm_time$Time_TimeZoneData$pacific_norfolk_l = _elm_community$elm_time$Time_TimeZoneData$unpack('Pacific/Norfolk|NMT NFT NFST NFT|-bc -bu -cu -b0|01213|-Kgbc W01G On0 1COp0|25e4');
var _elm_community$elm_time$Time_TimeZoneData$pacific_noumea_l = _elm_community$elm_time$Time_TimeZoneData$unpack('Pacific/Noumea|LMT NCT NCST|-b5.M -b0 -c0|01212121|-2l9n5.M 2EqM5.M xX0 1PB0 yn0 HeP0 Ao0|98e3');
var _elm_community$elm_time$Time_TimeZoneData$pacific_pago_pago_l = _elm_community$elm_time$Time_TimeZoneData$unpack('Pacific/Pago_Pago|LMT NST BST SST|bm.M b0 b0 b0|0123|-2nDMB.c 2gVzB.c EyM0|37e2');
var _elm_community$elm_time$Time_TimeZoneData$pacific_palau_l = _elm_community$elm_time$Time_TimeZoneData$unpack('Pacific/Palau|PWT|-90|0||21e3');
var _elm_community$elm_time$Time_TimeZoneData$pacific_pitcairn_l = _elm_community$elm_time$Time_TimeZoneData$unpack('Pacific/Pitcairn|PNT PST|8u 80|01|18Vku|56');
var _elm_community$elm_time$Time_TimeZoneData$pacific_pohnpei_l = _elm_community$elm_time$Time_TimeZoneData$unpack('Pacific/Pohnpei|PONT|-b0|0||34e3');
var _elm_community$elm_time$Time_TimeZoneData$pacific_port_moresby_l = _elm_community$elm_time$Time_TimeZoneData$unpack('Pacific/Port_Moresby|PGT|-a0|0||25e4');
var _elm_community$elm_time$Time_TimeZoneData$pacific_rarotonga_l = _elm_community$elm_time$Time_TimeZoneData$unpack('Pacific/Rarotonga|CKT CKHST CKT|au 9u a0|012121212121212121212121212|lyWu IL0 1zcu Onu 1zcu Onu 1zcu Rbu 1zcu Onu 1zcu Onu 1zcu Onu 1zcu Onu 1zcu Onu 1zcu Rbu 1zcu Onu 1zcu Onu 1zcu Onu|13e3');
var _elm_community$elm_time$Time_TimeZoneData$pacific_tahiti_l = _elm_community$elm_time$Time_TimeZoneData$unpack('Pacific/Tahiti|LMT TAHT|9W.g a0|01|-2joe1.I|18e4');
var _elm_community$elm_time$Time_TimeZoneData$pacific_tarawa_l = _elm_community$elm_time$Time_TimeZoneData$unpack('Pacific/Tarawa|GILT|-c0|0||29e3');
var _elm_community$elm_time$Time_TimeZoneData$pacific_tongatapu_l = _elm_community$elm_time$Time_TimeZoneData$unpack('Pacific/Tongatapu|+1220 +13 +14|-ck -d0 -e0|0121212121212121212121212121212121212121212121212121|-1aB0k 2n5dk 15A0 1wo0 xz0 1Q10 xz0 zWN0 s00 1VA0 uM0 1SM0 uM0 1SM0 uM0 1SM0 uM0 1VA0 s00 1VA0 s00 1VA0 uM0 1SM0 uM0 1SM0 uM0 1SM0 uM0 1VA0 s00 1VA0 uM0 1SM0 uM0 1SM0 uM0 1SM0 uM0 1VA0 s00 1VA0 s00 1VA0 uM0 1SM0 uM0 1SM0 uM0 1SM0 uM0|75e3');
var _elm_community$elm_time$Time_TimeZoneData$pacific_wake_l = _elm_community$elm_time$Time_TimeZoneData$unpack('Pacific/Wake|WAKT|-c0|0||16e3');
var _elm_community$elm_time$Time_TimeZoneData$pacific_wallis_l = _elm_community$elm_time$Time_TimeZoneData$unpack('Pacific/Wallis|WFT|-c0|0||94');
var _elm_community$elm_time$Time_TimeZoneData$pst8pdt_l = _elm_community$elm_time$Time_TimeZoneData$unpack('PST8PDT|PST PDT PWT PPT|80 70 70 70|010102301010101010101010101010101010101010101010101010101010101010101010101010101010101010101010101010101010101010101010101010101010101010101010101010|-261q0 1nX0 11B0 1nX0 SgN0 8x10 iy0 QwN0 1cL0 1cN0 1cL0 1cN0 1cL0 1cN0 1cL0 1cN0 1fz0 1cN0 1cL0 1cN0 1cL0 s10 1Vz0 LB0 1BX0 1cN0 1fz0 1a10 1fz0 1cN0 1cL0 1cN0 1cL0 1cN0 1cL0 1cN0 1cL0 1cN0 1fz0 1a10 1fz0 1cN0 1cL0 1cN0 1cL0 1cN0 1cL0 14p0 1lb0 14p0 1nX0 11B0 1nX0 11B0 1nX0 14p0 1lb0 14p0 1lb0 14p0 1nX0 11B0 1nX0 11B0 1nX0 14p0 1lb0 14p0 1lb0 14p0 1lb0 14p0 1nX0 11B0 1nX0 11B0 1nX0 14p0 1lb0 14p0 1lb0 14p0 1nX0 11B0 1nX0 11B0 1nX0 Rd0 1zb0 Op0 1zb0 Op0 1zb0 Rd0 1zb0 Op0 1zb0 Op0 1zb0 Op0 1zb0 Op0 1zb0 Op0 1zb0 Rd0 1zb0 Op0 1zb0 Op0 1zb0 Op0 1zb0 Op0 1zb0 Rd0 1zb0 Op0 1zb0 Op0 1zb0 Op0 1zb0 Op0 1zb0 Op0 1zb0 Rd0 1zb0 Op0 1zb0 Op0 1zb0 Op0 1zb0 Op0 1zb0 Rd0 1zb0 Op0 1zb0 Op0 1zb0 Op0 1zb0 Op0 1zb0 Op0 1zb0');
var _elm_community$elm_time$Time_TimeZoneData$wet_l = _elm_community$elm_time$Time_TimeZoneData$unpack('WET|WET WEST|0 -10|010101010101010101010101010101010101010101010101010101010101010101010101010101010101010101010101010101010101010101010101010|hDB0 1a00 1fA0 1cM0 1cM0 1cM0 1fA0 1a00 1cM0 1cM0 1cM0 1cM0 1cM0 1cM0 1cM0 1fA0 1cM0 1cM0 1cM0 1cM0 1cM0 1cM0 1cM0 1cM0 1cM0 1cM0 1cM0 1fA0 1cM0 1cM0 1cM0 1cM0 1cM0 1cM0 1cM0 1cM0 1cM0 1cM0 1fA0 1o00 11A0 1o00 11A0 1o00 11A0 1qM0 WM0 1qM0 WM0 1qM0 11A0 1o00 11A0 1o00 11A0 1qM0 WM0 1qM0 WM0 1qM0 WM0 1qM0 11A0 1o00 11A0 1o00 11A0 1qM0 WM0 1qM0 WM0 1qM0 11A0 1o00 11A0 1o00 11A0 1o00 11A0 1qM0 WM0 1qM0 WM0 1qM0 11A0 1o00 11A0 1o00 11A0 1qM0 WM0 1qM0 WM0 1qM0 11A0 1o00 11A0 1o00 11A0 1o00 11A0 1qM0 WM0 1qM0 WM0 1qM0 11A0 1o00 11A0 1o00 11A0 1qM0 WM0 1qM0 WM0 1qM0 WM0 1qM0 11A0 1o00 11A0 1o00');

var _elm_community$elm_time$Time_TimeZones$zulu = function (_p0) {
	var _p1 = _p0;
	return _elm_lang$lazy$Lazy$force(
		A2(_elm_community$elm_time$Time_TimeZoneData$link, 'Etc/UTC', _elm_community$elm_time$Time_TimeZoneData$etc_utc_l));
};
var _elm_community$elm_time$Time_TimeZones$wet = function (_p2) {
	var _p3 = _p2;
	return _elm_lang$lazy$Lazy$force(_elm_community$elm_time$Time_TimeZoneData$wet_l);
};
var _elm_community$elm_time$Time_TimeZones$w_su = function (_p4) {
	var _p5 = _p4;
	return _elm_lang$lazy$Lazy$force(
		A2(_elm_community$elm_time$Time_TimeZoneData$link, 'Europe/Moscow', _elm_community$elm_time$Time_TimeZoneData$europe_moscow_l));
};
var _elm_community$elm_time$Time_TimeZones$utc = function (_p6) {
	var _p7 = _p6;
	return _elm_lang$lazy$Lazy$force(
		A2(_elm_community$elm_time$Time_TimeZoneData$link, 'Etc/UTC', _elm_community$elm_time$Time_TimeZoneData$etc_utc_l));
};
var _elm_community$elm_time$Time_TimeZones$us_samoa = function (_p8) {
	var _p9 = _p8;
	return _elm_lang$lazy$Lazy$force(
		A2(_elm_community$elm_time$Time_TimeZoneData$link, 'Pacific/Pago_Pago', _elm_community$elm_time$Time_TimeZoneData$pacific_pago_pago_l));
};
var _elm_community$elm_time$Time_TimeZones$us_pacific_new = function (_p10) {
	var _p11 = _p10;
	return _elm_lang$lazy$Lazy$force(
		A2(_elm_community$elm_time$Time_TimeZoneData$link, 'America/Los_Angeles', _elm_community$elm_time$Time_TimeZoneData$america_los_angeles_l));
};
var _elm_community$elm_time$Time_TimeZones$us_pacific = function (_p12) {
	var _p13 = _p12;
	return _elm_lang$lazy$Lazy$force(
		A2(_elm_community$elm_time$Time_TimeZoneData$link, 'America/Los_Angeles', _elm_community$elm_time$Time_TimeZoneData$america_los_angeles_l));
};
var _elm_community$elm_time$Time_TimeZones$us_mountain = function (_p14) {
	var _p15 = _p14;
	return _elm_lang$lazy$Lazy$force(
		A2(_elm_community$elm_time$Time_TimeZoneData$link, 'America/Denver', _elm_community$elm_time$Time_TimeZoneData$america_denver_l));
};
var _elm_community$elm_time$Time_TimeZones$us_michigan = function (_p16) {
	var _p17 = _p16;
	return _elm_lang$lazy$Lazy$force(
		A2(_elm_community$elm_time$Time_TimeZoneData$link, 'America/Detroit', _elm_community$elm_time$Time_TimeZoneData$america_detroit_l));
};
var _elm_community$elm_time$Time_TimeZones$us_indiana_starke = function (_p18) {
	var _p19 = _p18;
	return _elm_lang$lazy$Lazy$force(
		A2(_elm_community$elm_time$Time_TimeZoneData$link, 'America/Indiana/Knox', _elm_community$elm_time$Time_TimeZoneData$america_indiana_knox_l));
};
var _elm_community$elm_time$Time_TimeZones$us_hawaii = function (_p20) {
	var _p21 = _p20;
	return _elm_lang$lazy$Lazy$force(
		A2(_elm_community$elm_time$Time_TimeZoneData$link, 'Pacific/Honolulu', _elm_community$elm_time$Time_TimeZoneData$pacific_honolulu_l));
};
var _elm_community$elm_time$Time_TimeZones$us_eastern = function (_p22) {
	var _p23 = _p22;
	return _elm_lang$lazy$Lazy$force(
		A2(_elm_community$elm_time$Time_TimeZoneData$link, 'America/New_York', _elm_community$elm_time$Time_TimeZoneData$america_new_york_l));
};
var _elm_community$elm_time$Time_TimeZones$us_east_indiana = function (_p24) {
	var _p25 = _p24;
	return _elm_lang$lazy$Lazy$force(
		A2(_elm_community$elm_time$Time_TimeZoneData$link, 'America/Fort_Wayne', _elm_community$elm_time$Time_TimeZoneData$america_fort_wayne_l));
};
var _elm_community$elm_time$Time_TimeZones$us_central = function (_p26) {
	var _p27 = _p26;
	return _elm_lang$lazy$Lazy$force(
		A2(_elm_community$elm_time$Time_TimeZoneData$link, 'America/Chicago', _elm_community$elm_time$Time_TimeZoneData$america_chicago_l));
};
var _elm_community$elm_time$Time_TimeZones$us_arizona = function (_p28) {
	var _p29 = _p28;
	return _elm_lang$lazy$Lazy$force(
		A2(_elm_community$elm_time$Time_TimeZoneData$link, 'America/Phoenix', _elm_community$elm_time$Time_TimeZoneData$america_phoenix_l));
};
var _elm_community$elm_time$Time_TimeZones$us_aleutian = function (_p30) {
	var _p31 = _p30;
	return _elm_lang$lazy$Lazy$force(
		A2(_elm_community$elm_time$Time_TimeZoneData$link, 'America/Adak', _elm_community$elm_time$Time_TimeZoneData$america_adak_l));
};
var _elm_community$elm_time$Time_TimeZones$us_alaska = function (_p32) {
	var _p33 = _p32;
	return _elm_lang$lazy$Lazy$force(
		A2(_elm_community$elm_time$Time_TimeZoneData$link, 'America/Anchorage', _elm_community$elm_time$Time_TimeZoneData$america_anchorage_l));
};
var _elm_community$elm_time$Time_TimeZones$universal = function (_p34) {
	var _p35 = _p34;
	return _elm_lang$lazy$Lazy$force(
		A2(_elm_community$elm_time$Time_TimeZoneData$link, 'Etc/UTC', _elm_community$elm_time$Time_TimeZoneData$etc_utc_l));
};
var _elm_community$elm_time$Time_TimeZones$uct = function (_p36) {
	var _p37 = _p36;
	return _elm_lang$lazy$Lazy$force(
		A2(_elm_community$elm_time$Time_TimeZoneData$link, 'Etc/UCT', _elm_community$elm_time$Time_TimeZoneData$etc_uct_l));
};
var _elm_community$elm_time$Time_TimeZones$turkey = function (_p38) {
	var _p39 = _p38;
	return _elm_lang$lazy$Lazy$force(
		A2(_elm_community$elm_time$Time_TimeZoneData$link, 'Europe/Istanbul', _elm_community$elm_time$Time_TimeZoneData$europe_istanbul_l));
};
var _elm_community$elm_time$Time_TimeZones$singapore = function (_p40) {
	var _p41 = _p40;
	return _elm_lang$lazy$Lazy$force(
		A2(_elm_community$elm_time$Time_TimeZoneData$link, 'Asia/Singapore', _elm_community$elm_time$Time_TimeZoneData$asia_singapore_l));
};
var _elm_community$elm_time$Time_TimeZones$rok = function (_p42) {
	var _p43 = _p42;
	return _elm_lang$lazy$Lazy$force(
		A2(_elm_community$elm_time$Time_TimeZoneData$link, 'Asia/Seoul', _elm_community$elm_time$Time_TimeZoneData$asia_seoul_l));
};
var _elm_community$elm_time$Time_TimeZones$roc = function (_p44) {
	var _p45 = _p44;
	return _elm_lang$lazy$Lazy$force(
		A2(_elm_community$elm_time$Time_TimeZoneData$link, 'Asia/Taipei', _elm_community$elm_time$Time_TimeZoneData$asia_taipei_l));
};
var _elm_community$elm_time$Time_TimeZones$pst8pdt = function (_p46) {
	var _p47 = _p46;
	return _elm_lang$lazy$Lazy$force(_elm_community$elm_time$Time_TimeZoneData$pst8pdt_l);
};
var _elm_community$elm_time$Time_TimeZones$prc = function (_p48) {
	var _p49 = _p48;
	return _elm_lang$lazy$Lazy$force(
		A2(_elm_community$elm_time$Time_TimeZoneData$link, 'Asia/Shanghai', _elm_community$elm_time$Time_TimeZoneData$asia_shanghai_l));
};
var _elm_community$elm_time$Time_TimeZones$portugal = function (_p50) {
	var _p51 = _p50;
	return _elm_lang$lazy$Lazy$force(
		A2(_elm_community$elm_time$Time_TimeZoneData$link, 'Europe/Lisbon', _elm_community$elm_time$Time_TimeZoneData$europe_lisbon_l));
};
var _elm_community$elm_time$Time_TimeZones$poland = function (_p52) {
	var _p53 = _p52;
	return _elm_lang$lazy$Lazy$force(
		A2(_elm_community$elm_time$Time_TimeZoneData$link, 'Europe/Warsaw', _elm_community$elm_time$Time_TimeZoneData$europe_warsaw_l));
};
var _elm_community$elm_time$Time_TimeZones$pacific_yap = function (_p54) {
	var _p55 = _p54;
	return _elm_lang$lazy$Lazy$force(
		A2(_elm_community$elm_time$Time_TimeZoneData$link, 'Pacific/Chuuk', _elm_community$elm_time$Time_TimeZoneData$pacific_chuuk_l));
};
var _elm_community$elm_time$Time_TimeZones$pacific_wallis = function (_p56) {
	var _p57 = _p56;
	return _elm_lang$lazy$Lazy$force(_elm_community$elm_time$Time_TimeZoneData$pacific_wallis_l);
};
var _elm_community$elm_time$Time_TimeZones$pacific_wake = function (_p58) {
	var _p59 = _p58;
	return _elm_lang$lazy$Lazy$force(_elm_community$elm_time$Time_TimeZoneData$pacific_wake_l);
};
var _elm_community$elm_time$Time_TimeZones$pacific_truk = function (_p60) {
	var _p61 = _p60;
	return _elm_lang$lazy$Lazy$force(
		A2(_elm_community$elm_time$Time_TimeZoneData$link, 'Pacific/Chuuk', _elm_community$elm_time$Time_TimeZoneData$pacific_chuuk_l));
};
var _elm_community$elm_time$Time_TimeZones$pacific_tongatapu = function (_p62) {
	var _p63 = _p62;
	return _elm_lang$lazy$Lazy$force(_elm_community$elm_time$Time_TimeZoneData$pacific_tongatapu_l);
};
var _elm_community$elm_time$Time_TimeZones$pacific_tarawa = function (_p64) {
	var _p65 = _p64;
	return _elm_lang$lazy$Lazy$force(_elm_community$elm_time$Time_TimeZoneData$pacific_tarawa_l);
};
var _elm_community$elm_time$Time_TimeZones$pacific_tahiti = function (_p66) {
	var _p67 = _p66;
	return _elm_lang$lazy$Lazy$force(_elm_community$elm_time$Time_TimeZoneData$pacific_tahiti_l);
};
var _elm_community$elm_time$Time_TimeZones$pacific_samoa = function (_p68) {
	var _p69 = _p68;
	return _elm_lang$lazy$Lazy$force(
		A2(_elm_community$elm_time$Time_TimeZoneData$link, 'Pacific/Pago_Pago', _elm_community$elm_time$Time_TimeZoneData$pacific_pago_pago_l));
};
var _elm_community$elm_time$Time_TimeZones$pacific_saipan = function (_p70) {
	var _p71 = _p70;
	return _elm_lang$lazy$Lazy$force(
		A2(_elm_community$elm_time$Time_TimeZoneData$link, 'Pacific/Guam', _elm_community$elm_time$Time_TimeZoneData$pacific_guam_l));
};
var _elm_community$elm_time$Time_TimeZones$pacific_rarotonga = function (_p72) {
	var _p73 = _p72;
	return _elm_lang$lazy$Lazy$force(_elm_community$elm_time$Time_TimeZoneData$pacific_rarotonga_l);
};
var _elm_community$elm_time$Time_TimeZones$pacific_port_moresby = function (_p74) {
	var _p75 = _p74;
	return _elm_lang$lazy$Lazy$force(_elm_community$elm_time$Time_TimeZoneData$pacific_port_moresby_l);
};
var _elm_community$elm_time$Time_TimeZones$pacific_ponape = function (_p76) {
	var _p77 = _p76;
	return _elm_lang$lazy$Lazy$force(
		A2(_elm_community$elm_time$Time_TimeZoneData$link, 'Pacific/Pohnpei', _elm_community$elm_time$Time_TimeZoneData$pacific_pohnpei_l));
};
var _elm_community$elm_time$Time_TimeZones$pacific_pohnpei = function (_p78) {
	var _p79 = _p78;
	return _elm_lang$lazy$Lazy$force(_elm_community$elm_time$Time_TimeZoneData$pacific_pohnpei_l);
};
var _elm_community$elm_time$Time_TimeZones$pacific_pitcairn = function (_p80) {
	var _p81 = _p80;
	return _elm_lang$lazy$Lazy$force(_elm_community$elm_time$Time_TimeZoneData$pacific_pitcairn_l);
};
var _elm_community$elm_time$Time_TimeZones$pacific_palau = function (_p82) {
	var _p83 = _p82;
	return _elm_lang$lazy$Lazy$force(_elm_community$elm_time$Time_TimeZoneData$pacific_palau_l);
};
var _elm_community$elm_time$Time_TimeZones$pacific_pago_pago = function (_p84) {
	var _p85 = _p84;
	return _elm_lang$lazy$Lazy$force(_elm_community$elm_time$Time_TimeZoneData$pacific_pago_pago_l);
};
var _elm_community$elm_time$Time_TimeZones$pacific_noumea = function (_p86) {
	var _p87 = _p86;
	return _elm_lang$lazy$Lazy$force(_elm_community$elm_time$Time_TimeZoneData$pacific_noumea_l);
};
var _elm_community$elm_time$Time_TimeZones$pacific_norfolk = function (_p88) {
	var _p89 = _p88;
	return _elm_lang$lazy$Lazy$force(_elm_community$elm_time$Time_TimeZoneData$pacific_norfolk_l);
};
var _elm_community$elm_time$Time_TimeZones$pacific_niue = function (_p90) {
	var _p91 = _p90;
	return _elm_lang$lazy$Lazy$force(_elm_community$elm_time$Time_TimeZoneData$pacific_niue_l);
};
var _elm_community$elm_time$Time_TimeZones$pacific_nauru = function (_p92) {
	var _p93 = _p92;
	return _elm_lang$lazy$Lazy$force(_elm_community$elm_time$Time_TimeZoneData$pacific_nauru_l);
};
var _elm_community$elm_time$Time_TimeZones$pacific_midway = function (_p94) {
	var _p95 = _p94;
	return _elm_lang$lazy$Lazy$force(
		A2(_elm_community$elm_time$Time_TimeZoneData$link, 'Pacific/Pago_Pago', _elm_community$elm_time$Time_TimeZoneData$pacific_pago_pago_l));
};
var _elm_community$elm_time$Time_TimeZones$pacific_marquesas = function (_p96) {
	var _p97 = _p96;
	return _elm_lang$lazy$Lazy$force(_elm_community$elm_time$Time_TimeZoneData$pacific_marquesas_l);
};
var _elm_community$elm_time$Time_TimeZones$pacific_majuro = function (_p98) {
	var _p99 = _p98;
	return _elm_lang$lazy$Lazy$force(_elm_community$elm_time$Time_TimeZoneData$pacific_majuro_l);
};
var _elm_community$elm_time$Time_TimeZones$pacific_kwajalein = function (_p100) {
	var _p101 = _p100;
	return _elm_lang$lazy$Lazy$force(_elm_community$elm_time$Time_TimeZoneData$pacific_kwajalein_l);
};
var _elm_community$elm_time$Time_TimeZones$pacific_kosrae = function (_p102) {
	var _p103 = _p102;
	return _elm_lang$lazy$Lazy$force(_elm_community$elm_time$Time_TimeZoneData$pacific_kosrae_l);
};
var _elm_community$elm_time$Time_TimeZones$pacific_kiritimati = function (_p104) {
	var _p105 = _p104;
	return _elm_lang$lazy$Lazy$force(_elm_community$elm_time$Time_TimeZoneData$pacific_kiritimati_l);
};
var _elm_community$elm_time$Time_TimeZones$pacific_johnston = function (_p106) {
	var _p107 = _p106;
	return _elm_lang$lazy$Lazy$force(
		A2(_elm_community$elm_time$Time_TimeZoneData$link, 'Pacific/Honolulu', _elm_community$elm_time$Time_TimeZoneData$pacific_honolulu_l));
};
var _elm_community$elm_time$Time_TimeZones$pacific_honolulu = function (_p108) {
	var _p109 = _p108;
	return _elm_lang$lazy$Lazy$force(_elm_community$elm_time$Time_TimeZoneData$pacific_honolulu_l);
};
var _elm_community$elm_time$Time_TimeZones$pacific_guam = function (_p110) {
	var _p111 = _p110;
	return _elm_lang$lazy$Lazy$force(_elm_community$elm_time$Time_TimeZoneData$pacific_guam_l);
};
var _elm_community$elm_time$Time_TimeZones$pacific_guadalcanal = function (_p112) {
	var _p113 = _p112;
	return _elm_lang$lazy$Lazy$force(_elm_community$elm_time$Time_TimeZoneData$pacific_guadalcanal_l);
};
var _elm_community$elm_time$Time_TimeZones$pacific_gambier = function (_p114) {
	var _p115 = _p114;
	return _elm_lang$lazy$Lazy$force(_elm_community$elm_time$Time_TimeZoneData$pacific_gambier_l);
};
var _elm_community$elm_time$Time_TimeZones$pacific_galapagos = function (_p116) {
	var _p117 = _p116;
	return _elm_lang$lazy$Lazy$force(_elm_community$elm_time$Time_TimeZoneData$pacific_galapagos_l);
};
var _elm_community$elm_time$Time_TimeZones$pacific_funafuti = function (_p118) {
	var _p119 = _p118;
	return _elm_lang$lazy$Lazy$force(_elm_community$elm_time$Time_TimeZoneData$pacific_funafuti_l);
};
var _elm_community$elm_time$Time_TimeZones$pacific_fiji = function (_p120) {
	var _p121 = _p120;
	return _elm_lang$lazy$Lazy$force(_elm_community$elm_time$Time_TimeZoneData$pacific_fiji_l);
};
var _elm_community$elm_time$Time_TimeZones$pacific_fakaofo = function (_p122) {
	var _p123 = _p122;
	return _elm_lang$lazy$Lazy$force(_elm_community$elm_time$Time_TimeZoneData$pacific_fakaofo_l);
};
var _elm_community$elm_time$Time_TimeZones$pacific_enderbury = function (_p124) {
	var _p125 = _p124;
	return _elm_lang$lazy$Lazy$force(_elm_community$elm_time$Time_TimeZoneData$pacific_enderbury_l);
};
var _elm_community$elm_time$Time_TimeZones$pacific_efate = function (_p126) {
	var _p127 = _p126;
	return _elm_lang$lazy$Lazy$force(_elm_community$elm_time$Time_TimeZoneData$pacific_efate_l);
};
var _elm_community$elm_time$Time_TimeZones$pacific_easter = function (_p128) {
	var _p129 = _p128;
	return _elm_lang$lazy$Lazy$force(_elm_community$elm_time$Time_TimeZoneData$pacific_easter_l);
};
var _elm_community$elm_time$Time_TimeZones$pacific_chuuk = function (_p130) {
	var _p131 = _p130;
	return _elm_lang$lazy$Lazy$force(_elm_community$elm_time$Time_TimeZoneData$pacific_chuuk_l);
};
var _elm_community$elm_time$Time_TimeZones$pacific_chatham = function (_p132) {
	var _p133 = _p132;
	return _elm_lang$lazy$Lazy$force(_elm_community$elm_time$Time_TimeZoneData$pacific_chatham_l);
};
var _elm_community$elm_time$Time_TimeZones$pacific_bougainville = function (_p134) {
	var _p135 = _p134;
	return _elm_lang$lazy$Lazy$force(_elm_community$elm_time$Time_TimeZoneData$pacific_bougainville_l);
};
var _elm_community$elm_time$Time_TimeZones$pacific_auckland = function (_p136) {
	var _p137 = _p136;
	return _elm_lang$lazy$Lazy$force(_elm_community$elm_time$Time_TimeZoneData$pacific_auckland_l);
};
var _elm_community$elm_time$Time_TimeZones$pacific_apia = function (_p138) {
	var _p139 = _p138;
	return _elm_lang$lazy$Lazy$force(_elm_community$elm_time$Time_TimeZoneData$pacific_apia_l);
};
var _elm_community$elm_time$Time_TimeZones$nz_chat = function (_p140) {
	var _p141 = _p140;
	return _elm_lang$lazy$Lazy$force(
		A2(_elm_community$elm_time$Time_TimeZoneData$link, 'Pacific/Chatham', _elm_community$elm_time$Time_TimeZoneData$pacific_chatham_l));
};
var _elm_community$elm_time$Time_TimeZones$nz = function (_p142) {
	var _p143 = _p142;
	return _elm_lang$lazy$Lazy$force(
		A2(_elm_community$elm_time$Time_TimeZoneData$link, 'Pacific/Auckland', _elm_community$elm_time$Time_TimeZoneData$pacific_auckland_l));
};
var _elm_community$elm_time$Time_TimeZones$navajo = function (_p144) {
	var _p145 = _p144;
	return _elm_lang$lazy$Lazy$force(
		A2(_elm_community$elm_time$Time_TimeZoneData$link, 'America/Denver', _elm_community$elm_time$Time_TimeZoneData$america_denver_l));
};
var _elm_community$elm_time$Time_TimeZones$mst7mdt = function (_p146) {
	var _p147 = _p146;
	return _elm_lang$lazy$Lazy$force(_elm_community$elm_time$Time_TimeZoneData$mst7mdt_l);
};
var _elm_community$elm_time$Time_TimeZones$mst = function (_p148) {
	var _p149 = _p148;
	return _elm_lang$lazy$Lazy$force(_elm_community$elm_time$Time_TimeZoneData$mst_l);
};
var _elm_community$elm_time$Time_TimeZones$mexico_general = function (_p150) {
	var _p151 = _p150;
	return _elm_lang$lazy$Lazy$force(
		A2(_elm_community$elm_time$Time_TimeZoneData$link, 'America/Mexico_City', _elm_community$elm_time$Time_TimeZoneData$america_mexico_city_l));
};
var _elm_community$elm_time$Time_TimeZones$mexico_bajasur = function (_p152) {
	var _p153 = _p152;
	return _elm_lang$lazy$Lazy$force(
		A2(_elm_community$elm_time$Time_TimeZoneData$link, 'America/Mazatlan', _elm_community$elm_time$Time_TimeZoneData$america_mazatlan_l));
};
var _elm_community$elm_time$Time_TimeZones$mexico_bajanorte = function (_p154) {
	var _p155 = _p154;
	return _elm_lang$lazy$Lazy$force(
		A2(_elm_community$elm_time$Time_TimeZoneData$link, 'America/Tijuana', _elm_community$elm_time$Time_TimeZoneData$america_tijuana_l));
};
var _elm_community$elm_time$Time_TimeZones$met = function (_p156) {
	var _p157 = _p156;
	return _elm_lang$lazy$Lazy$force(_elm_community$elm_time$Time_TimeZoneData$met_l);
};
var _elm_community$elm_time$Time_TimeZones$libya = function (_p158) {
	var _p159 = _p158;
	return _elm_lang$lazy$Lazy$force(
		A2(_elm_community$elm_time$Time_TimeZoneData$link, 'Africa/Tripoli', _elm_community$elm_time$Time_TimeZoneData$africa_tripoli_l));
};
var _elm_community$elm_time$Time_TimeZones$kwajalein = function (_p160) {
	var _p161 = _p160;
	return _elm_lang$lazy$Lazy$force(
		A2(_elm_community$elm_time$Time_TimeZoneData$link, 'Pacific/Kwajalein', _elm_community$elm_time$Time_TimeZoneData$pacific_kwajalein_l));
};
var _elm_community$elm_time$Time_TimeZones$japan = function (_p162) {
	var _p163 = _p162;
	return _elm_lang$lazy$Lazy$force(
		A2(_elm_community$elm_time$Time_TimeZoneData$link, 'Asia/Tokyo', _elm_community$elm_time$Time_TimeZoneData$asia_tokyo_l));
};
var _elm_community$elm_time$Time_TimeZones$jamaica = function (_p164) {
	var _p165 = _p164;
	return _elm_lang$lazy$Lazy$force(
		A2(_elm_community$elm_time$Time_TimeZoneData$link, 'America/Jamaica', _elm_community$elm_time$Time_TimeZoneData$america_jamaica_l));
};
var _elm_community$elm_time$Time_TimeZones$israel = function (_p166) {
	var _p167 = _p166;
	return _elm_lang$lazy$Lazy$force(
		A2(_elm_community$elm_time$Time_TimeZoneData$link, 'Asia/Jerusalem', _elm_community$elm_time$Time_TimeZoneData$asia_jerusalem_l));
};
var _elm_community$elm_time$Time_TimeZones$iran = function (_p168) {
	var _p169 = _p168;
	return _elm_lang$lazy$Lazy$force(
		A2(_elm_community$elm_time$Time_TimeZoneData$link, 'Asia/Tehran', _elm_community$elm_time$Time_TimeZoneData$asia_tehran_l));
};
var _elm_community$elm_time$Time_TimeZones$indian_reunion = function (_p170) {
	var _p171 = _p170;
	return _elm_lang$lazy$Lazy$force(_elm_community$elm_time$Time_TimeZoneData$indian_reunion_l);
};
var _elm_community$elm_time$Time_TimeZones$indian_mayotte = function (_p172) {
	var _p173 = _p172;
	return _elm_lang$lazy$Lazy$force(
		A2(_elm_community$elm_time$Time_TimeZoneData$link, 'Africa/Nairobi', _elm_community$elm_time$Time_TimeZoneData$africa_nairobi_l));
};
var _elm_community$elm_time$Time_TimeZones$indian_mauritius = function (_p174) {
	var _p175 = _p174;
	return _elm_lang$lazy$Lazy$force(_elm_community$elm_time$Time_TimeZoneData$indian_mauritius_l);
};
var _elm_community$elm_time$Time_TimeZones$indian_maldives = function (_p176) {
	var _p177 = _p176;
	return _elm_lang$lazy$Lazy$force(_elm_community$elm_time$Time_TimeZoneData$indian_maldives_l);
};
var _elm_community$elm_time$Time_TimeZones$indian_mahe = function (_p178) {
	var _p179 = _p178;
	return _elm_lang$lazy$Lazy$force(_elm_community$elm_time$Time_TimeZoneData$indian_mahe_l);
};
var _elm_community$elm_time$Time_TimeZones$indian_kerguelen = function (_p180) {
	var _p181 = _p180;
	return _elm_lang$lazy$Lazy$force(_elm_community$elm_time$Time_TimeZoneData$indian_kerguelen_l);
};
var _elm_community$elm_time$Time_TimeZones$indian_comoro = function (_p182) {
	var _p183 = _p182;
	return _elm_lang$lazy$Lazy$force(
		A2(_elm_community$elm_time$Time_TimeZoneData$link, 'Africa/Nairobi', _elm_community$elm_time$Time_TimeZoneData$africa_nairobi_l));
};
var _elm_community$elm_time$Time_TimeZones$indian_cocos = function (_p184) {
	var _p185 = _p184;
	return _elm_lang$lazy$Lazy$force(_elm_community$elm_time$Time_TimeZoneData$indian_cocos_l);
};
var _elm_community$elm_time$Time_TimeZones$indian_christmas = function (_p186) {
	var _p187 = _p186;
	return _elm_lang$lazy$Lazy$force(_elm_community$elm_time$Time_TimeZoneData$indian_christmas_l);
};
var _elm_community$elm_time$Time_TimeZones$indian_chagos = function (_p188) {
	var _p189 = _p188;
	return _elm_lang$lazy$Lazy$force(_elm_community$elm_time$Time_TimeZoneData$indian_chagos_l);
};
var _elm_community$elm_time$Time_TimeZones$indian_antananarivo = function (_p190) {
	var _p191 = _p190;
	return _elm_lang$lazy$Lazy$force(
		A2(_elm_community$elm_time$Time_TimeZoneData$link, 'Africa/Nairobi', _elm_community$elm_time$Time_TimeZoneData$africa_nairobi_l));
};
var _elm_community$elm_time$Time_TimeZones$iceland = function (_p192) {
	var _p193 = _p192;
	return _elm_lang$lazy$Lazy$force(
		A2(_elm_community$elm_time$Time_TimeZoneData$link, 'Atlantic/Reykjavik', _elm_community$elm_time$Time_TimeZoneData$atlantic_reykjavik_l));
};
var _elm_community$elm_time$Time_TimeZones$hst = function (_p194) {
	var _p195 = _p194;
	return _elm_lang$lazy$Lazy$force(_elm_community$elm_time$Time_TimeZoneData$hst_l);
};
var _elm_community$elm_time$Time_TimeZones$hongkong = function (_p196) {
	var _p197 = _p196;
	return _elm_lang$lazy$Lazy$force(
		A2(_elm_community$elm_time$Time_TimeZoneData$link, 'Asia/Hong_Kong', _elm_community$elm_time$Time_TimeZoneData$asia_hong_kong_l));
};
var _elm_community$elm_time$Time_TimeZones$greenwich = function (_p198) {
	var _p199 = _p198;
	return _elm_lang$lazy$Lazy$force(
		A2(_elm_community$elm_time$Time_TimeZoneData$link, 'Etc/GMT+0', _elm_community$elm_time$Time_TimeZoneData$etc_gmt_plus_0_l));
};
var _elm_community$elm_time$Time_TimeZones$gmt_plus_0 = function (_p200) {
	var _p201 = _p200;
	return _elm_lang$lazy$Lazy$force(
		A2(_elm_community$elm_time$Time_TimeZoneData$link, 'Etc/GMT+0', _elm_community$elm_time$Time_TimeZoneData$etc_gmt_plus_0_l));
};
var _elm_community$elm_time$Time_TimeZones$gmt_minus_0 = function (_p202) {
	var _p203 = _p202;
	return _elm_lang$lazy$Lazy$force(
		A2(_elm_community$elm_time$Time_TimeZoneData$link, 'Etc/GMT+0', _elm_community$elm_time$Time_TimeZoneData$etc_gmt_plus_0_l));
};
var _elm_community$elm_time$Time_TimeZones$gmt_0 = function (_p204) {
	var _p205 = _p204;
	return _elm_lang$lazy$Lazy$force(
		A2(_elm_community$elm_time$Time_TimeZoneData$link, 'Etc/GMT+0', _elm_community$elm_time$Time_TimeZoneData$etc_gmt_plus_0_l));
};
var _elm_community$elm_time$Time_TimeZones$gmt = function (_p206) {
	var _p207 = _p206;
	return _elm_lang$lazy$Lazy$force(
		A2(_elm_community$elm_time$Time_TimeZoneData$link, 'Etc/GMT+0', _elm_community$elm_time$Time_TimeZoneData$etc_gmt_plus_0_l));
};
var _elm_community$elm_time$Time_TimeZones$gb_eire = function (_p208) {
	var _p209 = _p208;
	return _elm_lang$lazy$Lazy$force(
		A2(_elm_community$elm_time$Time_TimeZoneData$link, 'Europe/London', _elm_community$elm_time$Time_TimeZoneData$europe_london_l));
};
var _elm_community$elm_time$Time_TimeZones$gb = function (_p210) {
	var _p211 = _p210;
	return _elm_lang$lazy$Lazy$force(
		A2(_elm_community$elm_time$Time_TimeZoneData$link, 'Europe/London', _elm_community$elm_time$Time_TimeZoneData$europe_london_l));
};
var _elm_community$elm_time$Time_TimeZones$europe_zurich = function (_p212) {
	var _p213 = _p212;
	return _elm_lang$lazy$Lazy$force(_elm_community$elm_time$Time_TimeZoneData$europe_zurich_l);
};
var _elm_community$elm_time$Time_TimeZones$europe_zaporozhye = function (_p214) {
	var _p215 = _p214;
	return _elm_lang$lazy$Lazy$force(_elm_community$elm_time$Time_TimeZoneData$europe_zaporozhye_l);
};
var _elm_community$elm_time$Time_TimeZones$europe_zagreb = function (_p216) {
	var _p217 = _p216;
	return _elm_lang$lazy$Lazy$force(
		A2(_elm_community$elm_time$Time_TimeZoneData$link, 'Europe/Belgrade', _elm_community$elm_time$Time_TimeZoneData$europe_belgrade_l));
};
var _elm_community$elm_time$Time_TimeZones$europe_warsaw = function (_p218) {
	var _p219 = _p218;
	return _elm_lang$lazy$Lazy$force(_elm_community$elm_time$Time_TimeZoneData$europe_warsaw_l);
};
var _elm_community$elm_time$Time_TimeZones$europe_volgograd = function (_p220) {
	var _p221 = _p220;
	return _elm_lang$lazy$Lazy$force(_elm_community$elm_time$Time_TimeZoneData$europe_volgograd_l);
};
var _elm_community$elm_time$Time_TimeZones$europe_vilnius = function (_p222) {
	var _p223 = _p222;
	return _elm_lang$lazy$Lazy$force(_elm_community$elm_time$Time_TimeZoneData$europe_vilnius_l);
};
var _elm_community$elm_time$Time_TimeZones$europe_vienna = function (_p224) {
	var _p225 = _p224;
	return _elm_lang$lazy$Lazy$force(_elm_community$elm_time$Time_TimeZoneData$europe_vienna_l);
};
var _elm_community$elm_time$Time_TimeZones$europe_vatican = function (_p226) {
	var _p227 = _p226;
	return _elm_lang$lazy$Lazy$force(
		A2(_elm_community$elm_time$Time_TimeZoneData$link, 'Europe/Rome', _elm_community$elm_time$Time_TimeZoneData$europe_rome_l));
};
var _elm_community$elm_time$Time_TimeZones$europe_vaduz = function (_p228) {
	var _p229 = _p228;
	return _elm_lang$lazy$Lazy$force(
		A2(_elm_community$elm_time$Time_TimeZoneData$link, 'Europe/Zurich', _elm_community$elm_time$Time_TimeZoneData$europe_zurich_l));
};
var _elm_community$elm_time$Time_TimeZones$europe_uzhgorod = function (_p230) {
	var _p231 = _p230;
	return _elm_lang$lazy$Lazy$force(_elm_community$elm_time$Time_TimeZoneData$europe_uzhgorod_l);
};
var _elm_community$elm_time$Time_TimeZones$europe_ulyanovsk = function (_p232) {
	var _p233 = _p232;
	return _elm_lang$lazy$Lazy$force(_elm_community$elm_time$Time_TimeZoneData$europe_ulyanovsk_l);
};
var _elm_community$elm_time$Time_TimeZones$europe_tiraspol = function (_p234) {
	var _p235 = _p234;
	return _elm_lang$lazy$Lazy$force(
		A2(_elm_community$elm_time$Time_TimeZoneData$link, 'Europe/Chisinau', _elm_community$elm_time$Time_TimeZoneData$europe_chisinau_l));
};
var _elm_community$elm_time$Time_TimeZones$europe_tirane = function (_p236) {
	var _p237 = _p236;
	return _elm_lang$lazy$Lazy$force(_elm_community$elm_time$Time_TimeZoneData$europe_tirane_l);
};
var _elm_community$elm_time$Time_TimeZones$europe_tallinn = function (_p238) {
	var _p239 = _p238;
	return _elm_lang$lazy$Lazy$force(_elm_community$elm_time$Time_TimeZoneData$europe_tallinn_l);
};
var _elm_community$elm_time$Time_TimeZones$europe_stockholm = function (_p240) {
	var _p241 = _p240;
	return _elm_lang$lazy$Lazy$force(_elm_community$elm_time$Time_TimeZoneData$europe_stockholm_l);
};
var _elm_community$elm_time$Time_TimeZones$europe_sofia = function (_p242) {
	var _p243 = _p242;
	return _elm_lang$lazy$Lazy$force(_elm_community$elm_time$Time_TimeZoneData$europe_sofia_l);
};
var _elm_community$elm_time$Time_TimeZones$europe_skopje = function (_p244) {
	var _p245 = _p244;
	return _elm_lang$lazy$Lazy$force(
		A2(_elm_community$elm_time$Time_TimeZoneData$link, 'Europe/Belgrade', _elm_community$elm_time$Time_TimeZoneData$europe_belgrade_l));
};
var _elm_community$elm_time$Time_TimeZones$europe_simferopol = function (_p246) {
	var _p247 = _p246;
	return _elm_lang$lazy$Lazy$force(_elm_community$elm_time$Time_TimeZoneData$europe_simferopol_l);
};
var _elm_community$elm_time$Time_TimeZones$europe_saratov = function (_p248) {
	var _p249 = _p248;
	return _elm_lang$lazy$Lazy$force(_elm_community$elm_time$Time_TimeZoneData$europe_saratov_l);
};
var _elm_community$elm_time$Time_TimeZones$europe_sarajevo = function (_p250) {
	var _p251 = _p250;
	return _elm_lang$lazy$Lazy$force(
		A2(_elm_community$elm_time$Time_TimeZoneData$link, 'Europe/Belgrade', _elm_community$elm_time$Time_TimeZoneData$europe_belgrade_l));
};
var _elm_community$elm_time$Time_TimeZones$europe_san_marino = function (_p252) {
	var _p253 = _p252;
	return _elm_lang$lazy$Lazy$force(
		A2(_elm_community$elm_time$Time_TimeZoneData$link, 'Europe/Rome', _elm_community$elm_time$Time_TimeZoneData$europe_rome_l));
};
var _elm_community$elm_time$Time_TimeZones$europe_samara = function (_p254) {
	var _p255 = _p254;
	return _elm_lang$lazy$Lazy$force(_elm_community$elm_time$Time_TimeZoneData$europe_samara_l);
};
var _elm_community$elm_time$Time_TimeZones$europe_rome = function (_p256) {
	var _p257 = _p256;
	return _elm_lang$lazy$Lazy$force(_elm_community$elm_time$Time_TimeZoneData$europe_rome_l);
};
var _elm_community$elm_time$Time_TimeZones$europe_riga = function (_p258) {
	var _p259 = _p258;
	return _elm_lang$lazy$Lazy$force(_elm_community$elm_time$Time_TimeZoneData$europe_riga_l);
};
var _elm_community$elm_time$Time_TimeZones$europe_prague = function (_p260) {
	var _p261 = _p260;
	return _elm_lang$lazy$Lazy$force(_elm_community$elm_time$Time_TimeZoneData$europe_prague_l);
};
var _elm_community$elm_time$Time_TimeZones$europe_podgorica = function (_p262) {
	var _p263 = _p262;
	return _elm_lang$lazy$Lazy$force(
		A2(_elm_community$elm_time$Time_TimeZoneData$link, 'Europe/Belgrade', _elm_community$elm_time$Time_TimeZoneData$europe_belgrade_l));
};
var _elm_community$elm_time$Time_TimeZones$europe_paris = function (_p264) {
	var _p265 = _p264;
	return _elm_lang$lazy$Lazy$force(_elm_community$elm_time$Time_TimeZoneData$europe_paris_l);
};
var _elm_community$elm_time$Time_TimeZones$europe_oslo = function (_p266) {
	var _p267 = _p266;
	return _elm_lang$lazy$Lazy$force(_elm_community$elm_time$Time_TimeZoneData$europe_oslo_l);
};
var _elm_community$elm_time$Time_TimeZones$europe_nicosia = function (_p268) {
	var _p269 = _p268;
	return _elm_lang$lazy$Lazy$force(
		A2(_elm_community$elm_time$Time_TimeZoneData$link, 'Asia/Nicosia', _elm_community$elm_time$Time_TimeZoneData$asia_nicosia_l));
};
var _elm_community$elm_time$Time_TimeZones$europe_moscow = function (_p270) {
	var _p271 = _p270;
	return _elm_lang$lazy$Lazy$force(_elm_community$elm_time$Time_TimeZoneData$europe_moscow_l);
};
var _elm_community$elm_time$Time_TimeZones$europe_monaco = function (_p272) {
	var _p273 = _p272;
	return _elm_lang$lazy$Lazy$force(_elm_community$elm_time$Time_TimeZoneData$europe_monaco_l);
};
var _elm_community$elm_time$Time_TimeZones$europe_minsk = function (_p274) {
	var _p275 = _p274;
	return _elm_lang$lazy$Lazy$force(_elm_community$elm_time$Time_TimeZoneData$europe_minsk_l);
};
var _elm_community$elm_time$Time_TimeZones$europe_mariehamn = function (_p276) {
	var _p277 = _p276;
	return _elm_lang$lazy$Lazy$force(
		A2(_elm_community$elm_time$Time_TimeZoneData$link, 'Europe/Helsinki', _elm_community$elm_time$Time_TimeZoneData$europe_helsinki_l));
};
var _elm_community$elm_time$Time_TimeZones$europe_malta = function (_p278) {
	var _p279 = _p278;
	return _elm_lang$lazy$Lazy$force(_elm_community$elm_time$Time_TimeZoneData$europe_malta_l);
};
var _elm_community$elm_time$Time_TimeZones$europe_madrid = function (_p280) {
	var _p281 = _p280;
	return _elm_lang$lazy$Lazy$force(_elm_community$elm_time$Time_TimeZoneData$europe_madrid_l);
};
var _elm_community$elm_time$Time_TimeZones$europe_luxembourg = function (_p282) {
	var _p283 = _p282;
	return _elm_lang$lazy$Lazy$force(_elm_community$elm_time$Time_TimeZoneData$europe_luxembourg_l);
};
var _elm_community$elm_time$Time_TimeZones$europe_london = function (_p284) {
	var _p285 = _p284;
	return _elm_lang$lazy$Lazy$force(_elm_community$elm_time$Time_TimeZoneData$europe_london_l);
};
var _elm_community$elm_time$Time_TimeZones$europe_ljubljana = function (_p286) {
	var _p287 = _p286;
	return _elm_lang$lazy$Lazy$force(
		A2(_elm_community$elm_time$Time_TimeZoneData$link, 'Europe/Belgrade', _elm_community$elm_time$Time_TimeZoneData$europe_belgrade_l));
};
var _elm_community$elm_time$Time_TimeZones$europe_lisbon = function (_p288) {
	var _p289 = _p288;
	return _elm_lang$lazy$Lazy$force(_elm_community$elm_time$Time_TimeZoneData$europe_lisbon_l);
};
var _elm_community$elm_time$Time_TimeZones$europe_kirov = function (_p290) {
	var _p291 = _p290;
	return _elm_lang$lazy$Lazy$force(_elm_community$elm_time$Time_TimeZoneData$europe_kirov_l);
};
var _elm_community$elm_time$Time_TimeZones$europe_kiev = function (_p292) {
	var _p293 = _p292;
	return _elm_lang$lazy$Lazy$force(_elm_community$elm_time$Time_TimeZoneData$europe_kiev_l);
};
var _elm_community$elm_time$Time_TimeZones$europe_kaliningrad = function (_p294) {
	var _p295 = _p294;
	return _elm_lang$lazy$Lazy$force(_elm_community$elm_time$Time_TimeZoneData$europe_kaliningrad_l);
};
var _elm_community$elm_time$Time_TimeZones$europe_jersey = function (_p296) {
	var _p297 = _p296;
	return _elm_lang$lazy$Lazy$force(
		A2(_elm_community$elm_time$Time_TimeZoneData$link, 'Europe/London', _elm_community$elm_time$Time_TimeZoneData$europe_london_l));
};
var _elm_community$elm_time$Time_TimeZones$europe_istanbul = function (_p298) {
	var _p299 = _p298;
	return _elm_lang$lazy$Lazy$force(_elm_community$elm_time$Time_TimeZoneData$europe_istanbul_l);
};
var _elm_community$elm_time$Time_TimeZones$europe_isle_of_man = function (_p300) {
	var _p301 = _p300;
	return _elm_lang$lazy$Lazy$force(
		A2(_elm_community$elm_time$Time_TimeZoneData$link, 'Europe/London', _elm_community$elm_time$Time_TimeZoneData$europe_london_l));
};
var _elm_community$elm_time$Time_TimeZones$europe_helsinki = function (_p302) {
	var _p303 = _p302;
	return _elm_lang$lazy$Lazy$force(_elm_community$elm_time$Time_TimeZoneData$europe_helsinki_l);
};
var _elm_community$elm_time$Time_TimeZones$europe_guernsey = function (_p304) {
	var _p305 = _p304;
	return _elm_lang$lazy$Lazy$force(
		A2(_elm_community$elm_time$Time_TimeZoneData$link, 'Europe/London', _elm_community$elm_time$Time_TimeZoneData$europe_london_l));
};
var _elm_community$elm_time$Time_TimeZones$europe_gibraltar = function (_p306) {
	var _p307 = _p306;
	return _elm_lang$lazy$Lazy$force(_elm_community$elm_time$Time_TimeZoneData$europe_gibraltar_l);
};
var _elm_community$elm_time$Time_TimeZones$europe_dublin = function (_p308) {
	var _p309 = _p308;
	return _elm_lang$lazy$Lazy$force(_elm_community$elm_time$Time_TimeZoneData$europe_dublin_l);
};
var _elm_community$elm_time$Time_TimeZones$europe_copenhagen = function (_p310) {
	var _p311 = _p310;
	return _elm_lang$lazy$Lazy$force(_elm_community$elm_time$Time_TimeZoneData$europe_copenhagen_l);
};
var _elm_community$elm_time$Time_TimeZones$europe_chisinau = function (_p312) {
	var _p313 = _p312;
	return _elm_lang$lazy$Lazy$force(_elm_community$elm_time$Time_TimeZoneData$europe_chisinau_l);
};
var _elm_community$elm_time$Time_TimeZones$europe_busingen = function (_p314) {
	var _p315 = _p314;
	return _elm_lang$lazy$Lazy$force(
		A2(_elm_community$elm_time$Time_TimeZoneData$link, 'Europe/Zurich', _elm_community$elm_time$Time_TimeZoneData$europe_zurich_l));
};
var _elm_community$elm_time$Time_TimeZones$europe_budapest = function (_p316) {
	var _p317 = _p316;
	return _elm_lang$lazy$Lazy$force(_elm_community$elm_time$Time_TimeZoneData$europe_budapest_l);
};
var _elm_community$elm_time$Time_TimeZones$europe_bucharest = function (_p318) {
	var _p319 = _p318;
	return _elm_lang$lazy$Lazy$force(_elm_community$elm_time$Time_TimeZoneData$europe_bucharest_l);
};
var _elm_community$elm_time$Time_TimeZones$europe_brussels = function (_p320) {
	var _p321 = _p320;
	return _elm_lang$lazy$Lazy$force(_elm_community$elm_time$Time_TimeZoneData$europe_brussels_l);
};
var _elm_community$elm_time$Time_TimeZones$europe_bratislava = function (_p322) {
	var _p323 = _p322;
	return _elm_lang$lazy$Lazy$force(
		A2(_elm_community$elm_time$Time_TimeZoneData$link, 'Europe/Prague', _elm_community$elm_time$Time_TimeZoneData$europe_prague_l));
};
var _elm_community$elm_time$Time_TimeZones$europe_berlin = function (_p324) {
	var _p325 = _p324;
	return _elm_lang$lazy$Lazy$force(_elm_community$elm_time$Time_TimeZoneData$europe_berlin_l);
};
var _elm_community$elm_time$Time_TimeZones$europe_belgrade = function (_p326) {
	var _p327 = _p326;
	return _elm_lang$lazy$Lazy$force(_elm_community$elm_time$Time_TimeZoneData$europe_belgrade_l);
};
var _elm_community$elm_time$Time_TimeZones$europe_belfast = function (_p328) {
	var _p329 = _p328;
	return _elm_lang$lazy$Lazy$force(
		A2(_elm_community$elm_time$Time_TimeZoneData$link, 'Europe/London', _elm_community$elm_time$Time_TimeZoneData$europe_london_l));
};
var _elm_community$elm_time$Time_TimeZones$europe_athens = function (_p330) {
	var _p331 = _p330;
	return _elm_lang$lazy$Lazy$force(_elm_community$elm_time$Time_TimeZoneData$europe_athens_l);
};
var _elm_community$elm_time$Time_TimeZones$europe_astrakhan = function (_p332) {
	var _p333 = _p332;
	return _elm_lang$lazy$Lazy$force(_elm_community$elm_time$Time_TimeZoneData$europe_astrakhan_l);
};
var _elm_community$elm_time$Time_TimeZones$europe_andorra = function (_p334) {
	var _p335 = _p334;
	return _elm_lang$lazy$Lazy$force(_elm_community$elm_time$Time_TimeZoneData$europe_andorra_l);
};
var _elm_community$elm_time$Time_TimeZones$europe_amsterdam = function (_p336) {
	var _p337 = _p336;
	return _elm_lang$lazy$Lazy$force(_elm_community$elm_time$Time_TimeZoneData$europe_amsterdam_l);
};
var _elm_community$elm_time$Time_TimeZones$etc_zulu = function (_p338) {
	var _p339 = _p338;
	return _elm_lang$lazy$Lazy$force(
		A2(_elm_community$elm_time$Time_TimeZoneData$link, 'Etc/UTC', _elm_community$elm_time$Time_TimeZoneData$etc_utc_l));
};
var _elm_community$elm_time$Time_TimeZones$etc_utc = function (_p340) {
	var _p341 = _p340;
	return _elm_lang$lazy$Lazy$force(_elm_community$elm_time$Time_TimeZoneData$etc_utc_l);
};
var _elm_community$elm_time$Time_TimeZones$etc_universal = function (_p342) {
	var _p343 = _p342;
	return _elm_lang$lazy$Lazy$force(
		A2(_elm_community$elm_time$Time_TimeZoneData$link, 'Etc/UTC', _elm_community$elm_time$Time_TimeZoneData$etc_utc_l));
};
var _elm_community$elm_time$Time_TimeZones$etc_uct = function (_p344) {
	var _p345 = _p344;
	return _elm_lang$lazy$Lazy$force(_elm_community$elm_time$Time_TimeZoneData$etc_uct_l);
};
var _elm_community$elm_time$Time_TimeZones$etc_greenwich = function (_p346) {
	var _p347 = _p346;
	return _elm_lang$lazy$Lazy$force(
		A2(_elm_community$elm_time$Time_TimeZoneData$link, 'Etc/GMT+0', _elm_community$elm_time$Time_TimeZoneData$etc_gmt_plus_0_l));
};
var _elm_community$elm_time$Time_TimeZones$etc_gmt_plus_9 = function (_p348) {
	var _p349 = _p348;
	return _elm_lang$lazy$Lazy$force(_elm_community$elm_time$Time_TimeZoneData$etc_gmt_plus_9_l);
};
var _elm_community$elm_time$Time_TimeZones$etc_gmt_plus_8 = function (_p350) {
	var _p351 = _p350;
	return _elm_lang$lazy$Lazy$force(_elm_community$elm_time$Time_TimeZoneData$etc_gmt_plus_8_l);
};
var _elm_community$elm_time$Time_TimeZones$etc_gmt_plus_7 = function (_p352) {
	var _p353 = _p352;
	return _elm_lang$lazy$Lazy$force(_elm_community$elm_time$Time_TimeZoneData$etc_gmt_plus_7_l);
};
var _elm_community$elm_time$Time_TimeZones$etc_gmt_plus_6 = function (_p354) {
	var _p355 = _p354;
	return _elm_lang$lazy$Lazy$force(_elm_community$elm_time$Time_TimeZoneData$etc_gmt_plus_6_l);
};
var _elm_community$elm_time$Time_TimeZones$etc_gmt_plus_5 = function (_p356) {
	var _p357 = _p356;
	return _elm_lang$lazy$Lazy$force(_elm_community$elm_time$Time_TimeZoneData$etc_gmt_plus_5_l);
};
var _elm_community$elm_time$Time_TimeZones$etc_gmt_plus_4 = function (_p358) {
	var _p359 = _p358;
	return _elm_lang$lazy$Lazy$force(_elm_community$elm_time$Time_TimeZoneData$etc_gmt_plus_4_l);
};
var _elm_community$elm_time$Time_TimeZones$etc_gmt_plus_3 = function (_p360) {
	var _p361 = _p360;
	return _elm_lang$lazy$Lazy$force(_elm_community$elm_time$Time_TimeZoneData$etc_gmt_plus_3_l);
};
var _elm_community$elm_time$Time_TimeZones$etc_gmt_plus_2 = function (_p362) {
	var _p363 = _p362;
	return _elm_lang$lazy$Lazy$force(_elm_community$elm_time$Time_TimeZoneData$etc_gmt_plus_2_l);
};
var _elm_community$elm_time$Time_TimeZones$etc_gmt_plus_12 = function (_p364) {
	var _p365 = _p364;
	return _elm_lang$lazy$Lazy$force(_elm_community$elm_time$Time_TimeZoneData$etc_gmt_plus_12_l);
};
var _elm_community$elm_time$Time_TimeZones$etc_gmt_plus_11 = function (_p366) {
	var _p367 = _p366;
	return _elm_lang$lazy$Lazy$force(_elm_community$elm_time$Time_TimeZoneData$etc_gmt_plus_11_l);
};
var _elm_community$elm_time$Time_TimeZones$etc_gmt_plus_10 = function (_p368) {
	var _p369 = _p368;
	return _elm_lang$lazy$Lazy$force(_elm_community$elm_time$Time_TimeZoneData$etc_gmt_plus_10_l);
};
var _elm_community$elm_time$Time_TimeZones$etc_gmt_plus_1 = function (_p370) {
	var _p371 = _p370;
	return _elm_lang$lazy$Lazy$force(_elm_community$elm_time$Time_TimeZoneData$etc_gmt_plus_1_l);
};
var _elm_community$elm_time$Time_TimeZones$etc_gmt_plus_0 = function (_p372) {
	var _p373 = _p372;
	return _elm_lang$lazy$Lazy$force(_elm_community$elm_time$Time_TimeZoneData$etc_gmt_plus_0_l);
};
var _elm_community$elm_time$Time_TimeZones$etc_gmt_minus_9 = function (_p374) {
	var _p375 = _p374;
	return _elm_lang$lazy$Lazy$force(_elm_community$elm_time$Time_TimeZoneData$etc_gmt_minus_9_l);
};
var _elm_community$elm_time$Time_TimeZones$etc_gmt_minus_8 = function (_p376) {
	var _p377 = _p376;
	return _elm_lang$lazy$Lazy$force(_elm_community$elm_time$Time_TimeZoneData$etc_gmt_minus_8_l);
};
var _elm_community$elm_time$Time_TimeZones$etc_gmt_minus_7 = function (_p378) {
	var _p379 = _p378;
	return _elm_lang$lazy$Lazy$force(_elm_community$elm_time$Time_TimeZoneData$etc_gmt_minus_7_l);
};
var _elm_community$elm_time$Time_TimeZones$etc_gmt_minus_6 = function (_p380) {
	var _p381 = _p380;
	return _elm_lang$lazy$Lazy$force(_elm_community$elm_time$Time_TimeZoneData$etc_gmt_minus_6_l);
};
var _elm_community$elm_time$Time_TimeZones$etc_gmt_minus_5 = function (_p382) {
	var _p383 = _p382;
	return _elm_lang$lazy$Lazy$force(_elm_community$elm_time$Time_TimeZoneData$etc_gmt_minus_5_l);
};
var _elm_community$elm_time$Time_TimeZones$etc_gmt_minus_4 = function (_p384) {
	var _p385 = _p384;
	return _elm_lang$lazy$Lazy$force(_elm_community$elm_time$Time_TimeZoneData$etc_gmt_minus_4_l);
};
var _elm_community$elm_time$Time_TimeZones$etc_gmt_minus_3 = function (_p386) {
	var _p387 = _p386;
	return _elm_lang$lazy$Lazy$force(_elm_community$elm_time$Time_TimeZoneData$etc_gmt_minus_3_l);
};
var _elm_community$elm_time$Time_TimeZones$etc_gmt_minus_2 = function (_p388) {
	var _p389 = _p388;
	return _elm_lang$lazy$Lazy$force(_elm_community$elm_time$Time_TimeZoneData$etc_gmt_minus_2_l);
};
var _elm_community$elm_time$Time_TimeZones$etc_gmt_minus_14 = function (_p390) {
	var _p391 = _p390;
	return _elm_lang$lazy$Lazy$force(_elm_community$elm_time$Time_TimeZoneData$etc_gmt_minus_14_l);
};
var _elm_community$elm_time$Time_TimeZones$etc_gmt_minus_13 = function (_p392) {
	var _p393 = _p392;
	return _elm_lang$lazy$Lazy$force(_elm_community$elm_time$Time_TimeZoneData$etc_gmt_minus_13_l);
};
var _elm_community$elm_time$Time_TimeZones$etc_gmt_minus_12 = function (_p394) {
	var _p395 = _p394;
	return _elm_lang$lazy$Lazy$force(_elm_community$elm_time$Time_TimeZoneData$etc_gmt_minus_12_l);
};
var _elm_community$elm_time$Time_TimeZones$etc_gmt_minus_11 = function (_p396) {
	var _p397 = _p396;
	return _elm_lang$lazy$Lazy$force(_elm_community$elm_time$Time_TimeZoneData$etc_gmt_minus_11_l);
};
var _elm_community$elm_time$Time_TimeZones$etc_gmt_minus_10 = function (_p398) {
	var _p399 = _p398;
	return _elm_lang$lazy$Lazy$force(_elm_community$elm_time$Time_TimeZoneData$etc_gmt_minus_10_l);
};
var _elm_community$elm_time$Time_TimeZones$etc_gmt_minus_1 = function (_p400) {
	var _p401 = _p400;
	return _elm_lang$lazy$Lazy$force(_elm_community$elm_time$Time_TimeZoneData$etc_gmt_minus_1_l);
};
var _elm_community$elm_time$Time_TimeZones$etc_gmt_minus_0 = function (_p402) {
	var _p403 = _p402;
	return _elm_lang$lazy$Lazy$force(
		A2(_elm_community$elm_time$Time_TimeZoneData$link, 'Etc/GMT+0', _elm_community$elm_time$Time_TimeZoneData$etc_gmt_plus_0_l));
};
var _elm_community$elm_time$Time_TimeZones$etc_gmt_0 = function (_p404) {
	var _p405 = _p404;
	return _elm_lang$lazy$Lazy$force(
		A2(_elm_community$elm_time$Time_TimeZoneData$link, 'Etc/GMT+0', _elm_community$elm_time$Time_TimeZoneData$etc_gmt_plus_0_l));
};
var _elm_community$elm_time$Time_TimeZones$etc_gmt = function (_p406) {
	var _p407 = _p406;
	return _elm_lang$lazy$Lazy$force(
		A2(_elm_community$elm_time$Time_TimeZoneData$link, 'Etc/GMT+0', _elm_community$elm_time$Time_TimeZoneData$etc_gmt_plus_0_l));
};
var _elm_community$elm_time$Time_TimeZones$est5edt = function (_p408) {
	var _p409 = _p408;
	return _elm_lang$lazy$Lazy$force(_elm_community$elm_time$Time_TimeZoneData$est5edt_l);
};
var _elm_community$elm_time$Time_TimeZones$est = function (_p410) {
	var _p411 = _p410;
	return _elm_lang$lazy$Lazy$force(_elm_community$elm_time$Time_TimeZoneData$est_l);
};
var _elm_community$elm_time$Time_TimeZones$eire = function (_p412) {
	var _p413 = _p412;
	return _elm_lang$lazy$Lazy$force(
		A2(_elm_community$elm_time$Time_TimeZoneData$link, 'Europe/Dublin', _elm_community$elm_time$Time_TimeZoneData$europe_dublin_l));
};
var _elm_community$elm_time$Time_TimeZones$egypt = function (_p414) {
	var _p415 = _p414;
	return _elm_lang$lazy$Lazy$force(
		A2(_elm_community$elm_time$Time_TimeZoneData$link, 'Africa/Cairo', _elm_community$elm_time$Time_TimeZoneData$africa_cairo_l));
};
var _elm_community$elm_time$Time_TimeZones$eet = function (_p416) {
	var _p417 = _p416;
	return _elm_lang$lazy$Lazy$force(_elm_community$elm_time$Time_TimeZoneData$eet_l);
};
var _elm_community$elm_time$Time_TimeZones$cuba = function (_p418) {
	var _p419 = _p418;
	return _elm_lang$lazy$Lazy$force(
		A2(_elm_community$elm_time$Time_TimeZoneData$link, 'America/Havana', _elm_community$elm_time$Time_TimeZoneData$america_havana_l));
};
var _elm_community$elm_time$Time_TimeZones$cst6cdt = function (_p420) {
	var _p421 = _p420;
	return _elm_lang$lazy$Lazy$force(_elm_community$elm_time$Time_TimeZoneData$cst6cdt_l);
};
var _elm_community$elm_time$Time_TimeZones$chile_easterisland = function (_p422) {
	var _p423 = _p422;
	return _elm_lang$lazy$Lazy$force(
		A2(_elm_community$elm_time$Time_TimeZoneData$link, 'Pacific/Easter', _elm_community$elm_time$Time_TimeZoneData$pacific_easter_l));
};
var _elm_community$elm_time$Time_TimeZones$chile_continental = function (_p424) {
	var _p425 = _p424;
	return _elm_lang$lazy$Lazy$force(
		A2(_elm_community$elm_time$Time_TimeZoneData$link, 'America/Santiago', _elm_community$elm_time$Time_TimeZoneData$america_santiago_l));
};
var _elm_community$elm_time$Time_TimeZones$cet = function (_p426) {
	var _p427 = _p426;
	return _elm_lang$lazy$Lazy$force(_elm_community$elm_time$Time_TimeZoneData$cet_l);
};
var _elm_community$elm_time$Time_TimeZones$canada_yukon = function (_p428) {
	var _p429 = _p428;
	return _elm_lang$lazy$Lazy$force(
		A2(_elm_community$elm_time$Time_TimeZoneData$link, 'America/Whitehorse', _elm_community$elm_time$Time_TimeZoneData$america_whitehorse_l));
};
var _elm_community$elm_time$Time_TimeZones$canada_saskatchewan = function (_p430) {
	var _p431 = _p430;
	return _elm_lang$lazy$Lazy$force(
		A2(_elm_community$elm_time$Time_TimeZoneData$link, 'America/Regina', _elm_community$elm_time$Time_TimeZoneData$america_regina_l));
};
var _elm_community$elm_time$Time_TimeZones$canada_pacific = function (_p432) {
	var _p433 = _p432;
	return _elm_lang$lazy$Lazy$force(
		A2(_elm_community$elm_time$Time_TimeZoneData$link, 'America/Vancouver', _elm_community$elm_time$Time_TimeZoneData$america_vancouver_l));
};
var _elm_community$elm_time$Time_TimeZones$canada_newfoundland = function (_p434) {
	var _p435 = _p434;
	return _elm_lang$lazy$Lazy$force(
		A2(_elm_community$elm_time$Time_TimeZoneData$link, 'America/St_Johns', _elm_community$elm_time$Time_TimeZoneData$america_st_johns_l));
};
var _elm_community$elm_time$Time_TimeZones$canada_mountain = function (_p436) {
	var _p437 = _p436;
	return _elm_lang$lazy$Lazy$force(
		A2(_elm_community$elm_time$Time_TimeZoneData$link, 'America/Edmonton', _elm_community$elm_time$Time_TimeZoneData$america_edmonton_l));
};
var _elm_community$elm_time$Time_TimeZones$canada_eastern = function (_p438) {
	var _p439 = _p438;
	return _elm_lang$lazy$Lazy$force(
		A2(_elm_community$elm_time$Time_TimeZoneData$link, 'America/Toronto', _elm_community$elm_time$Time_TimeZoneData$america_toronto_l));
};
var _elm_community$elm_time$Time_TimeZones$canada_east_saskatchewan = function (_p440) {
	var _p441 = _p440;
	return _elm_lang$lazy$Lazy$force(
		A2(_elm_community$elm_time$Time_TimeZoneData$link, 'America/Regina', _elm_community$elm_time$Time_TimeZoneData$america_regina_l));
};
var _elm_community$elm_time$Time_TimeZones$canada_central = function (_p442) {
	var _p443 = _p442;
	return _elm_lang$lazy$Lazy$force(
		A2(_elm_community$elm_time$Time_TimeZoneData$link, 'America/Winnipeg', _elm_community$elm_time$Time_TimeZoneData$america_winnipeg_l));
};
var _elm_community$elm_time$Time_TimeZones$canada_atlantic = function (_p444) {
	var _p445 = _p444;
	return _elm_lang$lazy$Lazy$force(
		A2(_elm_community$elm_time$Time_TimeZoneData$link, 'America/Halifax', _elm_community$elm_time$Time_TimeZoneData$america_halifax_l));
};
var _elm_community$elm_time$Time_TimeZones$brazil_west = function (_p446) {
	var _p447 = _p446;
	return _elm_lang$lazy$Lazy$force(
		A2(_elm_community$elm_time$Time_TimeZoneData$link, 'America/Manaus', _elm_community$elm_time$Time_TimeZoneData$america_manaus_l));
};
var _elm_community$elm_time$Time_TimeZones$brazil_east = function (_p448) {
	var _p449 = _p448;
	return _elm_lang$lazy$Lazy$force(
		A2(_elm_community$elm_time$Time_TimeZoneData$link, 'America/Sao_Paulo', _elm_community$elm_time$Time_TimeZoneData$america_sao_paulo_l));
};
var _elm_community$elm_time$Time_TimeZones$brazil_denoronha = function (_p450) {
	var _p451 = _p450;
	return _elm_lang$lazy$Lazy$force(
		A2(_elm_community$elm_time$Time_TimeZoneData$link, 'America/Noronha', _elm_community$elm_time$Time_TimeZoneData$america_noronha_l));
};
var _elm_community$elm_time$Time_TimeZones$brazil_acre = function (_p452) {
	var _p453 = _p452;
	return _elm_lang$lazy$Lazy$force(
		A2(_elm_community$elm_time$Time_TimeZoneData$link, 'America/Rio_Branco', _elm_community$elm_time$Time_TimeZoneData$america_rio_branco_l));
};
var _elm_community$elm_time$Time_TimeZones$australia_yancowinna = function (_p454) {
	var _p455 = _p454;
	return _elm_lang$lazy$Lazy$force(
		A2(_elm_community$elm_time$Time_TimeZoneData$link, 'Australia/Broken_Hill', _elm_community$elm_time$Time_TimeZoneData$australia_broken_hill_l));
};
var _elm_community$elm_time$Time_TimeZones$australia_west = function (_p456) {
	var _p457 = _p456;
	return _elm_lang$lazy$Lazy$force(
		A2(_elm_community$elm_time$Time_TimeZoneData$link, 'Australia/Perth', _elm_community$elm_time$Time_TimeZoneData$australia_perth_l));
};
var _elm_community$elm_time$Time_TimeZones$australia_victoria = function (_p458) {
	var _p459 = _p458;
	return _elm_lang$lazy$Lazy$force(
		A2(_elm_community$elm_time$Time_TimeZoneData$link, 'Australia/Melbourne', _elm_community$elm_time$Time_TimeZoneData$australia_melbourne_l));
};
var _elm_community$elm_time$Time_TimeZones$australia_tasmania = function (_p460) {
	var _p461 = _p460;
	return _elm_lang$lazy$Lazy$force(
		A2(_elm_community$elm_time$Time_TimeZoneData$link, 'Australia/Hobart', _elm_community$elm_time$Time_TimeZoneData$australia_hobart_l));
};
var _elm_community$elm_time$Time_TimeZones$australia_sydney = function (_p462) {
	var _p463 = _p462;
	return _elm_lang$lazy$Lazy$force(_elm_community$elm_time$Time_TimeZoneData$australia_sydney_l);
};
var _elm_community$elm_time$Time_TimeZones$australia_south = function (_p464) {
	var _p465 = _p464;
	return _elm_lang$lazy$Lazy$force(
		A2(_elm_community$elm_time$Time_TimeZoneData$link, 'Australia/Adelaide', _elm_community$elm_time$Time_TimeZoneData$australia_adelaide_l));
};
var _elm_community$elm_time$Time_TimeZones$australia_queensland = function (_p466) {
	var _p467 = _p466;
	return _elm_lang$lazy$Lazy$force(
		A2(_elm_community$elm_time$Time_TimeZoneData$link, 'Australia/Brisbane', _elm_community$elm_time$Time_TimeZoneData$australia_brisbane_l));
};
var _elm_community$elm_time$Time_TimeZones$australia_perth = function (_p468) {
	var _p469 = _p468;
	return _elm_lang$lazy$Lazy$force(_elm_community$elm_time$Time_TimeZoneData$australia_perth_l);
};
var _elm_community$elm_time$Time_TimeZones$australia_nsw = function (_p470) {
	var _p471 = _p470;
	return _elm_lang$lazy$Lazy$force(
		A2(_elm_community$elm_time$Time_TimeZoneData$link, 'Australia/Sydney', _elm_community$elm_time$Time_TimeZoneData$australia_sydney_l));
};
var _elm_community$elm_time$Time_TimeZones$australia_north = function (_p472) {
	var _p473 = _p472;
	return _elm_lang$lazy$Lazy$force(
		A2(_elm_community$elm_time$Time_TimeZoneData$link, 'Australia/Darwin', _elm_community$elm_time$Time_TimeZoneData$australia_darwin_l));
};
var _elm_community$elm_time$Time_TimeZones$australia_melbourne = function (_p474) {
	var _p475 = _p474;
	return _elm_lang$lazy$Lazy$force(_elm_community$elm_time$Time_TimeZoneData$australia_melbourne_l);
};
var _elm_community$elm_time$Time_TimeZones$australia_lord_howe = function (_p476) {
	var _p477 = _p476;
	return _elm_lang$lazy$Lazy$force(_elm_community$elm_time$Time_TimeZoneData$australia_lord_howe_l);
};
var _elm_community$elm_time$Time_TimeZones$australia_lindeman = function (_p478) {
	var _p479 = _p478;
	return _elm_lang$lazy$Lazy$force(_elm_community$elm_time$Time_TimeZoneData$australia_lindeman_l);
};
var _elm_community$elm_time$Time_TimeZones$australia_lhi = function (_p480) {
	var _p481 = _p480;
	return _elm_lang$lazy$Lazy$force(
		A2(_elm_community$elm_time$Time_TimeZoneData$link, 'Australia/Lord_Howe', _elm_community$elm_time$Time_TimeZoneData$australia_lord_howe_l));
};
var _elm_community$elm_time$Time_TimeZones$australia_hobart = function (_p482) {
	var _p483 = _p482;
	return _elm_lang$lazy$Lazy$force(_elm_community$elm_time$Time_TimeZoneData$australia_hobart_l);
};
var _elm_community$elm_time$Time_TimeZones$australia_eucla = function (_p484) {
	var _p485 = _p484;
	return _elm_lang$lazy$Lazy$force(_elm_community$elm_time$Time_TimeZoneData$australia_eucla_l);
};
var _elm_community$elm_time$Time_TimeZones$australia_darwin = function (_p486) {
	var _p487 = _p486;
	return _elm_lang$lazy$Lazy$force(_elm_community$elm_time$Time_TimeZoneData$australia_darwin_l);
};
var _elm_community$elm_time$Time_TimeZones$australia_currie = function (_p488) {
	var _p489 = _p488;
	return _elm_lang$lazy$Lazy$force(_elm_community$elm_time$Time_TimeZoneData$australia_currie_l);
};
var _elm_community$elm_time$Time_TimeZones$australia_canberra = function (_p490) {
	var _p491 = _p490;
	return _elm_lang$lazy$Lazy$force(
		A2(_elm_community$elm_time$Time_TimeZoneData$link, 'Australia/Sydney', _elm_community$elm_time$Time_TimeZoneData$australia_sydney_l));
};
var _elm_community$elm_time$Time_TimeZones$australia_broken_hill = function (_p492) {
	var _p493 = _p492;
	return _elm_lang$lazy$Lazy$force(_elm_community$elm_time$Time_TimeZoneData$australia_broken_hill_l);
};
var _elm_community$elm_time$Time_TimeZones$australia_brisbane = function (_p494) {
	var _p495 = _p494;
	return _elm_lang$lazy$Lazy$force(_elm_community$elm_time$Time_TimeZoneData$australia_brisbane_l);
};
var _elm_community$elm_time$Time_TimeZones$australia_adelaide = function (_p496) {
	var _p497 = _p496;
	return _elm_lang$lazy$Lazy$force(_elm_community$elm_time$Time_TimeZoneData$australia_adelaide_l);
};
var _elm_community$elm_time$Time_TimeZones$australia_act = function (_p498) {
	var _p499 = _p498;
	return _elm_lang$lazy$Lazy$force(
		A2(_elm_community$elm_time$Time_TimeZoneData$link, 'Australia/Sydney', _elm_community$elm_time$Time_TimeZoneData$australia_sydney_l));
};
var _elm_community$elm_time$Time_TimeZones$atlantic_stanley = function (_p500) {
	var _p501 = _p500;
	return _elm_lang$lazy$Lazy$force(_elm_community$elm_time$Time_TimeZoneData$atlantic_stanley_l);
};
var _elm_community$elm_time$Time_TimeZones$atlantic_st_helena = function (_p502) {
	var _p503 = _p502;
	return _elm_lang$lazy$Lazy$force(
		A2(_elm_community$elm_time$Time_TimeZoneData$link, 'Africa/Abidjan', _elm_community$elm_time$Time_TimeZoneData$africa_abidjan_l));
};
var _elm_community$elm_time$Time_TimeZones$atlantic_south_georgia = function (_p504) {
	var _p505 = _p504;
	return _elm_lang$lazy$Lazy$force(_elm_community$elm_time$Time_TimeZoneData$atlantic_south_georgia_l);
};
var _elm_community$elm_time$Time_TimeZones$atlantic_reykjavik = function (_p506) {
	var _p507 = _p506;
	return _elm_lang$lazy$Lazy$force(_elm_community$elm_time$Time_TimeZoneData$atlantic_reykjavik_l);
};
var _elm_community$elm_time$Time_TimeZones$atlantic_madeira = function (_p508) {
	var _p509 = _p508;
	return _elm_lang$lazy$Lazy$force(_elm_community$elm_time$Time_TimeZoneData$atlantic_madeira_l);
};
var _elm_community$elm_time$Time_TimeZones$atlantic_jan_mayen = function (_p510) {
	var _p511 = _p510;
	return _elm_lang$lazy$Lazy$force(
		A2(_elm_community$elm_time$Time_TimeZoneData$link, 'Europe/Oslo', _elm_community$elm_time$Time_TimeZoneData$europe_oslo_l));
};
var _elm_community$elm_time$Time_TimeZones$atlantic_faroe = function (_p512) {
	var _p513 = _p512;
	return _elm_lang$lazy$Lazy$force(_elm_community$elm_time$Time_TimeZoneData$atlantic_faroe_l);
};
var _elm_community$elm_time$Time_TimeZones$atlantic_faeroe = function (_p514) {
	var _p515 = _p514;
	return _elm_lang$lazy$Lazy$force(
		A2(_elm_community$elm_time$Time_TimeZoneData$link, 'Atlantic/Faroe', _elm_community$elm_time$Time_TimeZoneData$atlantic_faroe_l));
};
var _elm_community$elm_time$Time_TimeZones$atlantic_cape_verde = function (_p516) {
	var _p517 = _p516;
	return _elm_lang$lazy$Lazy$force(_elm_community$elm_time$Time_TimeZoneData$atlantic_cape_verde_l);
};
var _elm_community$elm_time$Time_TimeZones$atlantic_canary = function (_p518) {
	var _p519 = _p518;
	return _elm_lang$lazy$Lazy$force(_elm_community$elm_time$Time_TimeZoneData$atlantic_canary_l);
};
var _elm_community$elm_time$Time_TimeZones$atlantic_bermuda = function (_p520) {
	var _p521 = _p520;
	return _elm_lang$lazy$Lazy$force(_elm_community$elm_time$Time_TimeZoneData$atlantic_bermuda_l);
};
var _elm_community$elm_time$Time_TimeZones$atlantic_azores = function (_p522) {
	var _p523 = _p522;
	return _elm_lang$lazy$Lazy$force(_elm_community$elm_time$Time_TimeZoneData$atlantic_azores_l);
};
var _elm_community$elm_time$Time_TimeZones$asia_yerevan = function (_p524) {
	var _p525 = _p524;
	return _elm_lang$lazy$Lazy$force(_elm_community$elm_time$Time_TimeZoneData$asia_yerevan_l);
};
var _elm_community$elm_time$Time_TimeZones$asia_yekaterinburg = function (_p526) {
	var _p527 = _p526;
	return _elm_lang$lazy$Lazy$force(_elm_community$elm_time$Time_TimeZoneData$asia_yekaterinburg_l);
};
var _elm_community$elm_time$Time_TimeZones$asia_yangon = function (_p528) {
	var _p529 = _p528;
	return _elm_lang$lazy$Lazy$force(
		A2(_elm_community$elm_time$Time_TimeZoneData$link, 'Asia/Rangoon', _elm_community$elm_time$Time_TimeZoneData$asia_rangoon_l));
};
var _elm_community$elm_time$Time_TimeZones$asia_yakutsk = function (_p530) {
	var _p531 = _p530;
	return _elm_lang$lazy$Lazy$force(_elm_community$elm_time$Time_TimeZoneData$asia_yakutsk_l);
};
var _elm_community$elm_time$Time_TimeZones$asia_vladivostok = function (_p532) {
	var _p533 = _p532;
	return _elm_lang$lazy$Lazy$force(_elm_community$elm_time$Time_TimeZoneData$asia_vladivostok_l);
};
var _elm_community$elm_time$Time_TimeZones$asia_vientiane = function (_p534) {
	var _p535 = _p534;
	return _elm_lang$lazy$Lazy$force(
		A2(_elm_community$elm_time$Time_TimeZoneData$link, 'Asia/Bangkok', _elm_community$elm_time$Time_TimeZoneData$asia_bangkok_l));
};
var _elm_community$elm_time$Time_TimeZones$asia_ust_nera = function (_p536) {
	var _p537 = _p536;
	return _elm_lang$lazy$Lazy$force(_elm_community$elm_time$Time_TimeZoneData$asia_ust_nera_l);
};
var _elm_community$elm_time$Time_TimeZones$asia_urumqi = function (_p538) {
	var _p539 = _p538;
	return _elm_lang$lazy$Lazy$force(_elm_community$elm_time$Time_TimeZoneData$asia_urumqi_l);
};
var _elm_community$elm_time$Time_TimeZones$asia_ulan_bator = function (_p540) {
	var _p541 = _p540;
	return _elm_lang$lazy$Lazy$force(
		A2(_elm_community$elm_time$Time_TimeZoneData$link, 'Asia/Ulaanbaatar', _elm_community$elm_time$Time_TimeZoneData$asia_ulaanbaatar_l));
};
var _elm_community$elm_time$Time_TimeZones$asia_ulaanbaatar = function (_p542) {
	var _p543 = _p542;
	return _elm_lang$lazy$Lazy$force(_elm_community$elm_time$Time_TimeZoneData$asia_ulaanbaatar_l);
};
var _elm_community$elm_time$Time_TimeZones$asia_ujung_pandang = function (_p544) {
	var _p545 = _p544;
	return _elm_lang$lazy$Lazy$force(
		A2(_elm_community$elm_time$Time_TimeZoneData$link, 'Asia/Makassar', _elm_community$elm_time$Time_TimeZoneData$asia_makassar_l));
};
var _elm_community$elm_time$Time_TimeZones$asia_tomsk = function (_p546) {
	var _p547 = _p546;
	return _elm_lang$lazy$Lazy$force(_elm_community$elm_time$Time_TimeZoneData$asia_tomsk_l);
};
var _elm_community$elm_time$Time_TimeZones$asia_tokyo = function (_p548) {
	var _p549 = _p548;
	return _elm_lang$lazy$Lazy$force(_elm_community$elm_time$Time_TimeZoneData$asia_tokyo_l);
};
var _elm_community$elm_time$Time_TimeZones$asia_thimphu = function (_p550) {
	var _p551 = _p550;
	return _elm_lang$lazy$Lazy$force(_elm_community$elm_time$Time_TimeZoneData$asia_thimphu_l);
};
var _elm_community$elm_time$Time_TimeZones$asia_thimbu = function (_p552) {
	var _p553 = _p552;
	return _elm_lang$lazy$Lazy$force(
		A2(_elm_community$elm_time$Time_TimeZoneData$link, 'Asia/Thimphu', _elm_community$elm_time$Time_TimeZoneData$asia_thimphu_l));
};
var _elm_community$elm_time$Time_TimeZones$asia_tel_aviv = function (_p554) {
	var _p555 = _p554;
	return _elm_lang$lazy$Lazy$force(
		A2(_elm_community$elm_time$Time_TimeZoneData$link, 'Asia/Jerusalem', _elm_community$elm_time$Time_TimeZoneData$asia_jerusalem_l));
};
var _elm_community$elm_time$Time_TimeZones$asia_tehran = function (_p556) {
	var _p557 = _p556;
	return _elm_lang$lazy$Lazy$force(_elm_community$elm_time$Time_TimeZoneData$asia_tehran_l);
};
var _elm_community$elm_time$Time_TimeZones$asia_tbilisi = function (_p558) {
	var _p559 = _p558;
	return _elm_lang$lazy$Lazy$force(_elm_community$elm_time$Time_TimeZoneData$asia_tbilisi_l);
};
var _elm_community$elm_time$Time_TimeZones$asia_tashkent = function (_p560) {
	var _p561 = _p560;
	return _elm_lang$lazy$Lazy$force(_elm_community$elm_time$Time_TimeZoneData$asia_tashkent_l);
};
var _elm_community$elm_time$Time_TimeZones$asia_taipei = function (_p562) {
	var _p563 = _p562;
	return _elm_lang$lazy$Lazy$force(_elm_community$elm_time$Time_TimeZoneData$asia_taipei_l);
};
var _elm_community$elm_time$Time_TimeZones$asia_srednekolymsk = function (_p564) {
	var _p565 = _p564;
	return _elm_lang$lazy$Lazy$force(_elm_community$elm_time$Time_TimeZoneData$asia_srednekolymsk_l);
};
var _elm_community$elm_time$Time_TimeZones$asia_singapore = function (_p566) {
	var _p567 = _p566;
	return _elm_lang$lazy$Lazy$force(_elm_community$elm_time$Time_TimeZoneData$asia_singapore_l);
};
var _elm_community$elm_time$Time_TimeZones$asia_shanghai = function (_p568) {
	var _p569 = _p568;
	return _elm_lang$lazy$Lazy$force(_elm_community$elm_time$Time_TimeZoneData$asia_shanghai_l);
};
var _elm_community$elm_time$Time_TimeZones$asia_seoul = function (_p570) {
	var _p571 = _p570;
	return _elm_lang$lazy$Lazy$force(_elm_community$elm_time$Time_TimeZoneData$asia_seoul_l);
};
var _elm_community$elm_time$Time_TimeZones$asia_samarkand = function (_p572) {
	var _p573 = _p572;
	return _elm_lang$lazy$Lazy$force(_elm_community$elm_time$Time_TimeZoneData$asia_samarkand_l);
};
var _elm_community$elm_time$Time_TimeZones$asia_sakhalin = function (_p574) {
	var _p575 = _p574;
	return _elm_lang$lazy$Lazy$force(_elm_community$elm_time$Time_TimeZoneData$asia_sakhalin_l);
};
var _elm_community$elm_time$Time_TimeZones$asia_saigon = function (_p576) {
	var _p577 = _p576;
	return _elm_lang$lazy$Lazy$force(
		A2(_elm_community$elm_time$Time_TimeZoneData$link, 'Asia/Ho_Chi_Minh', _elm_community$elm_time$Time_TimeZoneData$asia_ho_chi_minh_l));
};
var _elm_community$elm_time$Time_TimeZones$asia_riyadh = function (_p578) {
	var _p579 = _p578;
	return _elm_lang$lazy$Lazy$force(_elm_community$elm_time$Time_TimeZoneData$asia_riyadh_l);
};
var _elm_community$elm_time$Time_TimeZones$asia_rangoon = function (_p580) {
	var _p581 = _p580;
	return _elm_lang$lazy$Lazy$force(_elm_community$elm_time$Time_TimeZoneData$asia_rangoon_l);
};
var _elm_community$elm_time$Time_TimeZones$asia_qyzylorda = function (_p582) {
	var _p583 = _p582;
	return _elm_lang$lazy$Lazy$force(_elm_community$elm_time$Time_TimeZoneData$asia_qyzylorda_l);
};
var _elm_community$elm_time$Time_TimeZones$asia_qatar = function (_p584) {
	var _p585 = _p584;
	return _elm_lang$lazy$Lazy$force(_elm_community$elm_time$Time_TimeZoneData$asia_qatar_l);
};
var _elm_community$elm_time$Time_TimeZones$asia_pyongyang = function (_p586) {
	var _p587 = _p586;
	return _elm_lang$lazy$Lazy$force(_elm_community$elm_time$Time_TimeZoneData$asia_pyongyang_l);
};
var _elm_community$elm_time$Time_TimeZones$asia_pontianak = function (_p588) {
	var _p589 = _p588;
	return _elm_lang$lazy$Lazy$force(_elm_community$elm_time$Time_TimeZoneData$asia_pontianak_l);
};
var _elm_community$elm_time$Time_TimeZones$asia_phnom_penh = function (_p590) {
	var _p591 = _p590;
	return _elm_lang$lazy$Lazy$force(
		A2(_elm_community$elm_time$Time_TimeZoneData$link, 'Asia/Bangkok', _elm_community$elm_time$Time_TimeZoneData$asia_bangkok_l));
};
var _elm_community$elm_time$Time_TimeZones$asia_oral = function (_p592) {
	var _p593 = _p592;
	return _elm_lang$lazy$Lazy$force(_elm_community$elm_time$Time_TimeZoneData$asia_oral_l);
};
var _elm_community$elm_time$Time_TimeZones$asia_omsk = function (_p594) {
	var _p595 = _p594;
	return _elm_lang$lazy$Lazy$force(_elm_community$elm_time$Time_TimeZoneData$asia_omsk_l);
};
var _elm_community$elm_time$Time_TimeZones$asia_novosibirsk = function (_p596) {
	var _p597 = _p596;
	return _elm_lang$lazy$Lazy$force(_elm_community$elm_time$Time_TimeZoneData$asia_novosibirsk_l);
};
var _elm_community$elm_time$Time_TimeZones$asia_novokuznetsk = function (_p598) {
	var _p599 = _p598;
	return _elm_lang$lazy$Lazy$force(_elm_community$elm_time$Time_TimeZoneData$asia_novokuznetsk_l);
};
var _elm_community$elm_time$Time_TimeZones$asia_nicosia = function (_p600) {
	var _p601 = _p600;
	return _elm_lang$lazy$Lazy$force(_elm_community$elm_time$Time_TimeZoneData$asia_nicosia_l);
};
var _elm_community$elm_time$Time_TimeZones$asia_muscat = function (_p602) {
	var _p603 = _p602;
	return _elm_lang$lazy$Lazy$force(
		A2(_elm_community$elm_time$Time_TimeZoneData$link, 'Asia/Dubai', _elm_community$elm_time$Time_TimeZoneData$asia_dubai_l));
};
var _elm_community$elm_time$Time_TimeZones$asia_manila = function (_p604) {
	var _p605 = _p604;
	return _elm_lang$lazy$Lazy$force(_elm_community$elm_time$Time_TimeZoneData$asia_manila_l);
};
var _elm_community$elm_time$Time_TimeZones$asia_makassar = function (_p606) {
	var _p607 = _p606;
	return _elm_lang$lazy$Lazy$force(_elm_community$elm_time$Time_TimeZoneData$asia_makassar_l);
};
var _elm_community$elm_time$Time_TimeZones$asia_magadan = function (_p608) {
	var _p609 = _p608;
	return _elm_lang$lazy$Lazy$force(_elm_community$elm_time$Time_TimeZoneData$asia_magadan_l);
};
var _elm_community$elm_time$Time_TimeZones$asia_macau = function (_p610) {
	var _p611 = _p610;
	return _elm_lang$lazy$Lazy$force(_elm_community$elm_time$Time_TimeZoneData$asia_macau_l);
};
var _elm_community$elm_time$Time_TimeZones$asia_macao = function (_p612) {
	var _p613 = _p612;
	return _elm_lang$lazy$Lazy$force(
		A2(_elm_community$elm_time$Time_TimeZoneData$link, 'Asia/Macau', _elm_community$elm_time$Time_TimeZoneData$asia_macau_l));
};
var _elm_community$elm_time$Time_TimeZones$asia_kuwait = function (_p614) {
	var _p615 = _p614;
	return _elm_lang$lazy$Lazy$force(
		A2(_elm_community$elm_time$Time_TimeZoneData$link, 'Asia/Riyadh', _elm_community$elm_time$Time_TimeZoneData$asia_riyadh_l));
};
var _elm_community$elm_time$Time_TimeZones$asia_kuching = function (_p616) {
	var _p617 = _p616;
	return _elm_lang$lazy$Lazy$force(_elm_community$elm_time$Time_TimeZoneData$asia_kuching_l);
};
var _elm_community$elm_time$Time_TimeZones$asia_kuala_lumpur = function (_p618) {
	var _p619 = _p618;
	return _elm_lang$lazy$Lazy$force(_elm_community$elm_time$Time_TimeZoneData$asia_kuala_lumpur_l);
};
var _elm_community$elm_time$Time_TimeZones$asia_krasnoyarsk = function (_p620) {
	var _p621 = _p620;
	return _elm_lang$lazy$Lazy$force(_elm_community$elm_time$Time_TimeZoneData$asia_krasnoyarsk_l);
};
var _elm_community$elm_time$Time_TimeZones$asia_kolkata = function (_p622) {
	var _p623 = _p622;
	return _elm_lang$lazy$Lazy$force(_elm_community$elm_time$Time_TimeZoneData$asia_kolkata_l);
};
var _elm_community$elm_time$Time_TimeZones$asia_khandyga = function (_p624) {
	var _p625 = _p624;
	return _elm_lang$lazy$Lazy$force(_elm_community$elm_time$Time_TimeZoneData$asia_khandyga_l);
};
var _elm_community$elm_time$Time_TimeZones$asia_katmandu = function (_p626) {
	var _p627 = _p626;
	return _elm_lang$lazy$Lazy$force(
		A2(_elm_community$elm_time$Time_TimeZoneData$link, 'Asia/Kathmandu', _elm_community$elm_time$Time_TimeZoneData$asia_kathmandu_l));
};
var _elm_community$elm_time$Time_TimeZones$asia_kathmandu = function (_p628) {
	var _p629 = _p628;
	return _elm_lang$lazy$Lazy$force(_elm_community$elm_time$Time_TimeZoneData$asia_kathmandu_l);
};
var _elm_community$elm_time$Time_TimeZones$asia_kashgar = function (_p630) {
	var _p631 = _p630;
	return _elm_lang$lazy$Lazy$force(
		A2(_elm_community$elm_time$Time_TimeZoneData$link, 'Asia/Urumqi', _elm_community$elm_time$Time_TimeZoneData$asia_urumqi_l));
};
var _elm_community$elm_time$Time_TimeZones$asia_karachi = function (_p632) {
	var _p633 = _p632;
	return _elm_lang$lazy$Lazy$force(_elm_community$elm_time$Time_TimeZoneData$asia_karachi_l);
};
var _elm_community$elm_time$Time_TimeZones$asia_kamchatka = function (_p634) {
	var _p635 = _p634;
	return _elm_lang$lazy$Lazy$force(_elm_community$elm_time$Time_TimeZoneData$asia_kamchatka_l);
};
var _elm_community$elm_time$Time_TimeZones$asia_kabul = function (_p636) {
	var _p637 = _p636;
	return _elm_lang$lazy$Lazy$force(_elm_community$elm_time$Time_TimeZoneData$asia_kabul_l);
};
var _elm_community$elm_time$Time_TimeZones$asia_jerusalem = function (_p638) {
	var _p639 = _p638;
	return _elm_lang$lazy$Lazy$force(_elm_community$elm_time$Time_TimeZoneData$asia_jerusalem_l);
};
var _elm_community$elm_time$Time_TimeZones$asia_jayapura = function (_p640) {
	var _p641 = _p640;
	return _elm_lang$lazy$Lazy$force(_elm_community$elm_time$Time_TimeZoneData$asia_jayapura_l);
};
var _elm_community$elm_time$Time_TimeZones$asia_jakarta = function (_p642) {
	var _p643 = _p642;
	return _elm_lang$lazy$Lazy$force(_elm_community$elm_time$Time_TimeZoneData$asia_jakarta_l);
};
var _elm_community$elm_time$Time_TimeZones$asia_istanbul = function (_p644) {
	var _p645 = _p644;
	return _elm_lang$lazy$Lazy$force(
		A2(_elm_community$elm_time$Time_TimeZoneData$link, 'Europe/Istanbul', _elm_community$elm_time$Time_TimeZoneData$europe_istanbul_l));
};
var _elm_community$elm_time$Time_TimeZones$asia_irkutsk = function (_p646) {
	var _p647 = _p646;
	return _elm_lang$lazy$Lazy$force(_elm_community$elm_time$Time_TimeZoneData$asia_irkutsk_l);
};
var _elm_community$elm_time$Time_TimeZones$asia_hovd = function (_p648) {
	var _p649 = _p648;
	return _elm_lang$lazy$Lazy$force(_elm_community$elm_time$Time_TimeZoneData$asia_hovd_l);
};
var _elm_community$elm_time$Time_TimeZones$asia_hong_kong = function (_p650) {
	var _p651 = _p650;
	return _elm_lang$lazy$Lazy$force(_elm_community$elm_time$Time_TimeZoneData$asia_hong_kong_l);
};
var _elm_community$elm_time$Time_TimeZones$asia_ho_chi_minh = function (_p652) {
	var _p653 = _p652;
	return _elm_lang$lazy$Lazy$force(_elm_community$elm_time$Time_TimeZoneData$asia_ho_chi_minh_l);
};
var _elm_community$elm_time$Time_TimeZones$asia_hebron = function (_p654) {
	var _p655 = _p654;
	return _elm_lang$lazy$Lazy$force(_elm_community$elm_time$Time_TimeZoneData$asia_hebron_l);
};
var _elm_community$elm_time$Time_TimeZones$asia_harbin = function (_p656) {
	var _p657 = _p656;
	return _elm_lang$lazy$Lazy$force(
		A2(_elm_community$elm_time$Time_TimeZoneData$link, 'Asia/Shanghai', _elm_community$elm_time$Time_TimeZoneData$asia_shanghai_l));
};
var _elm_community$elm_time$Time_TimeZones$asia_gaza = function (_p658) {
	var _p659 = _p658;
	return _elm_lang$lazy$Lazy$force(_elm_community$elm_time$Time_TimeZoneData$asia_gaza_l);
};
var _elm_community$elm_time$Time_TimeZones$asia_famagusta = function (_p660) {
	var _p661 = _p660;
	return _elm_lang$lazy$Lazy$force(_elm_community$elm_time$Time_TimeZoneData$asia_famagusta_l);
};
var _elm_community$elm_time$Time_TimeZones$asia_dushanbe = function (_p662) {
	var _p663 = _p662;
	return _elm_lang$lazy$Lazy$force(_elm_community$elm_time$Time_TimeZoneData$asia_dushanbe_l);
};
var _elm_community$elm_time$Time_TimeZones$asia_dubai = function (_p664) {
	var _p665 = _p664;
	return _elm_lang$lazy$Lazy$force(_elm_community$elm_time$Time_TimeZoneData$asia_dubai_l);
};
var _elm_community$elm_time$Time_TimeZones$asia_dili = function (_p666) {
	var _p667 = _p666;
	return _elm_lang$lazy$Lazy$force(_elm_community$elm_time$Time_TimeZoneData$asia_dili_l);
};
var _elm_community$elm_time$Time_TimeZones$asia_dhaka = function (_p668) {
	var _p669 = _p668;
	return _elm_lang$lazy$Lazy$force(_elm_community$elm_time$Time_TimeZoneData$asia_dhaka_l);
};
var _elm_community$elm_time$Time_TimeZones$asia_damascus = function (_p670) {
	var _p671 = _p670;
	return _elm_lang$lazy$Lazy$force(_elm_community$elm_time$Time_TimeZoneData$asia_damascus_l);
};
var _elm_community$elm_time$Time_TimeZones$asia_dacca = function (_p672) {
	var _p673 = _p672;
	return _elm_lang$lazy$Lazy$force(
		A2(_elm_community$elm_time$Time_TimeZoneData$link, 'Asia/Dhaka', _elm_community$elm_time$Time_TimeZoneData$asia_dhaka_l));
};
var _elm_community$elm_time$Time_TimeZones$asia_colombo = function (_p674) {
	var _p675 = _p674;
	return _elm_lang$lazy$Lazy$force(_elm_community$elm_time$Time_TimeZoneData$asia_colombo_l);
};
var _elm_community$elm_time$Time_TimeZones$asia_chungking = function (_p676) {
	var _p677 = _p676;
	return _elm_lang$lazy$Lazy$force(
		A2(_elm_community$elm_time$Time_TimeZoneData$link, 'Asia/Shanghai', _elm_community$elm_time$Time_TimeZoneData$asia_shanghai_l));
};
var _elm_community$elm_time$Time_TimeZones$asia_chongqing = function (_p678) {
	var _p679 = _p678;
	return _elm_lang$lazy$Lazy$force(
		A2(_elm_community$elm_time$Time_TimeZoneData$link, 'Asia/Shanghai', _elm_community$elm_time$Time_TimeZoneData$asia_shanghai_l));
};
var _elm_community$elm_time$Time_TimeZones$asia_choibalsan = function (_p680) {
	var _p681 = _p680;
	return _elm_lang$lazy$Lazy$force(_elm_community$elm_time$Time_TimeZoneData$asia_choibalsan_l);
};
var _elm_community$elm_time$Time_TimeZones$asia_chita = function (_p682) {
	var _p683 = _p682;
	return _elm_lang$lazy$Lazy$force(_elm_community$elm_time$Time_TimeZoneData$asia_chita_l);
};
var _elm_community$elm_time$Time_TimeZones$asia_calcutta = function (_p684) {
	var _p685 = _p684;
	return _elm_lang$lazy$Lazy$force(
		A2(_elm_community$elm_time$Time_TimeZoneData$link, 'Asia/Kolkata', _elm_community$elm_time$Time_TimeZoneData$asia_kolkata_l));
};
var _elm_community$elm_time$Time_TimeZones$asia_brunei = function (_p686) {
	var _p687 = _p686;
	return _elm_lang$lazy$Lazy$force(_elm_community$elm_time$Time_TimeZoneData$asia_brunei_l);
};
var _elm_community$elm_time$Time_TimeZones$asia_bishkek = function (_p688) {
	var _p689 = _p688;
	return _elm_lang$lazy$Lazy$force(_elm_community$elm_time$Time_TimeZoneData$asia_bishkek_l);
};
var _elm_community$elm_time$Time_TimeZones$asia_beirut = function (_p690) {
	var _p691 = _p690;
	return _elm_lang$lazy$Lazy$force(_elm_community$elm_time$Time_TimeZoneData$asia_beirut_l);
};
var _elm_community$elm_time$Time_TimeZones$asia_barnaul = function (_p692) {
	var _p693 = _p692;
	return _elm_lang$lazy$Lazy$force(_elm_community$elm_time$Time_TimeZoneData$asia_barnaul_l);
};
var _elm_community$elm_time$Time_TimeZones$asia_bangkok = function (_p694) {
	var _p695 = _p694;
	return _elm_lang$lazy$Lazy$force(_elm_community$elm_time$Time_TimeZoneData$asia_bangkok_l);
};
var _elm_community$elm_time$Time_TimeZones$asia_baku = function (_p696) {
	var _p697 = _p696;
	return _elm_lang$lazy$Lazy$force(_elm_community$elm_time$Time_TimeZoneData$asia_baku_l);
};
var _elm_community$elm_time$Time_TimeZones$asia_bahrain = function (_p698) {
	var _p699 = _p698;
	return _elm_lang$lazy$Lazy$force(
		A2(_elm_community$elm_time$Time_TimeZoneData$link, 'Asia/Qatar', _elm_community$elm_time$Time_TimeZoneData$asia_qatar_l));
};
var _elm_community$elm_time$Time_TimeZones$asia_baghdad = function (_p700) {
	var _p701 = _p700;
	return _elm_lang$lazy$Lazy$force(_elm_community$elm_time$Time_TimeZoneData$asia_baghdad_l);
};
var _elm_community$elm_time$Time_TimeZones$asia_atyrau = function (_p702) {
	var _p703 = _p702;
	return _elm_lang$lazy$Lazy$force(_elm_community$elm_time$Time_TimeZoneData$asia_atyrau_l);
};
var _elm_community$elm_time$Time_TimeZones$asia_ashkhabad = function (_p704) {
	var _p705 = _p704;
	return _elm_lang$lazy$Lazy$force(
		A2(_elm_community$elm_time$Time_TimeZoneData$link, 'Asia/Ashgabat', _elm_community$elm_time$Time_TimeZoneData$asia_ashgabat_l));
};
var _elm_community$elm_time$Time_TimeZones$asia_ashgabat = function (_p706) {
	var _p707 = _p706;
	return _elm_lang$lazy$Lazy$force(_elm_community$elm_time$Time_TimeZoneData$asia_ashgabat_l);
};
var _elm_community$elm_time$Time_TimeZones$asia_aqtobe = function (_p708) {
	var _p709 = _p708;
	return _elm_lang$lazy$Lazy$force(_elm_community$elm_time$Time_TimeZoneData$asia_aqtobe_l);
};
var _elm_community$elm_time$Time_TimeZones$asia_aqtau = function (_p710) {
	var _p711 = _p710;
	return _elm_lang$lazy$Lazy$force(_elm_community$elm_time$Time_TimeZoneData$asia_aqtau_l);
};
var _elm_community$elm_time$Time_TimeZones$asia_anadyr = function (_p712) {
	var _p713 = _p712;
	return _elm_lang$lazy$Lazy$force(_elm_community$elm_time$Time_TimeZoneData$asia_anadyr_l);
};
var _elm_community$elm_time$Time_TimeZones$asia_amman = function (_p714) {
	var _p715 = _p714;
	return _elm_lang$lazy$Lazy$force(_elm_community$elm_time$Time_TimeZoneData$asia_amman_l);
};
var _elm_community$elm_time$Time_TimeZones$asia_almaty = function (_p716) {
	var _p717 = _p716;
	return _elm_lang$lazy$Lazy$force(_elm_community$elm_time$Time_TimeZoneData$asia_almaty_l);
};
var _elm_community$elm_time$Time_TimeZones$asia_aden = function (_p718) {
	var _p719 = _p718;
	return _elm_lang$lazy$Lazy$force(
		A2(_elm_community$elm_time$Time_TimeZoneData$link, 'Asia/Riyadh', _elm_community$elm_time$Time_TimeZoneData$asia_riyadh_l));
};
var _elm_community$elm_time$Time_TimeZones$arctic_longyearbyen = function (_p720) {
	var _p721 = _p720;
	return _elm_lang$lazy$Lazy$force(
		A2(_elm_community$elm_time$Time_TimeZoneData$link, 'Europe/Oslo', _elm_community$elm_time$Time_TimeZoneData$europe_oslo_l));
};
var _elm_community$elm_time$Time_TimeZones$antarctica_vostok = function (_p722) {
	var _p723 = _p722;
	return _elm_lang$lazy$Lazy$force(_elm_community$elm_time$Time_TimeZoneData$antarctica_vostok_l);
};
var _elm_community$elm_time$Time_TimeZones$antarctica_troll = function (_p724) {
	var _p725 = _p724;
	return _elm_lang$lazy$Lazy$force(_elm_community$elm_time$Time_TimeZoneData$antarctica_troll_l);
};
var _elm_community$elm_time$Time_TimeZones$antarctica_syowa = function (_p726) {
	var _p727 = _p726;
	return _elm_lang$lazy$Lazy$force(_elm_community$elm_time$Time_TimeZoneData$antarctica_syowa_l);
};
var _elm_community$elm_time$Time_TimeZones$antarctica_south_pole = function (_p728) {
	var _p729 = _p728;
	return _elm_lang$lazy$Lazy$force(
		A2(_elm_community$elm_time$Time_TimeZoneData$link, 'Pacific/Auckland', _elm_community$elm_time$Time_TimeZoneData$pacific_auckland_l));
};
var _elm_community$elm_time$Time_TimeZones$antarctica_rothera = function (_p730) {
	var _p731 = _p730;
	return _elm_lang$lazy$Lazy$force(_elm_community$elm_time$Time_TimeZoneData$antarctica_rothera_l);
};
var _elm_community$elm_time$Time_TimeZones$antarctica_palmer = function (_p732) {
	var _p733 = _p732;
	return _elm_lang$lazy$Lazy$force(_elm_community$elm_time$Time_TimeZoneData$antarctica_palmer_l);
};
var _elm_community$elm_time$Time_TimeZones$antarctica_mcmurdo = function (_p734) {
	var _p735 = _p734;
	return _elm_lang$lazy$Lazy$force(
		A2(_elm_community$elm_time$Time_TimeZoneData$link, 'Pacific/Auckland', _elm_community$elm_time$Time_TimeZoneData$pacific_auckland_l));
};
var _elm_community$elm_time$Time_TimeZones$antarctica_mawson = function (_p736) {
	var _p737 = _p736;
	return _elm_lang$lazy$Lazy$force(_elm_community$elm_time$Time_TimeZoneData$antarctica_mawson_l);
};
var _elm_community$elm_time$Time_TimeZones$antarctica_macquarie = function (_p738) {
	var _p739 = _p738;
	return _elm_lang$lazy$Lazy$force(_elm_community$elm_time$Time_TimeZoneData$antarctica_macquarie_l);
};
var _elm_community$elm_time$Time_TimeZones$antarctica_dumontdurville = function (_p740) {
	var _p741 = _p740;
	return _elm_lang$lazy$Lazy$force(_elm_community$elm_time$Time_TimeZoneData$antarctica_dumontdurville_l);
};
var _elm_community$elm_time$Time_TimeZones$antarctica_davis = function (_p742) {
	var _p743 = _p742;
	return _elm_lang$lazy$Lazy$force(_elm_community$elm_time$Time_TimeZoneData$antarctica_davis_l);
};
var _elm_community$elm_time$Time_TimeZones$antarctica_casey = function (_p744) {
	var _p745 = _p744;
	return _elm_lang$lazy$Lazy$force(_elm_community$elm_time$Time_TimeZoneData$antarctica_casey_l);
};
var _elm_community$elm_time$Time_TimeZones$america_yellowknife = function (_p746) {
	var _p747 = _p746;
	return _elm_lang$lazy$Lazy$force(_elm_community$elm_time$Time_TimeZoneData$america_yellowknife_l);
};
var _elm_community$elm_time$Time_TimeZones$america_yakutat = function (_p748) {
	var _p749 = _p748;
	return _elm_lang$lazy$Lazy$force(_elm_community$elm_time$Time_TimeZoneData$america_yakutat_l);
};
var _elm_community$elm_time$Time_TimeZones$america_winnipeg = function (_p750) {
	var _p751 = _p750;
	return _elm_lang$lazy$Lazy$force(_elm_community$elm_time$Time_TimeZoneData$america_winnipeg_l);
};
var _elm_community$elm_time$Time_TimeZones$america_whitehorse = function (_p752) {
	var _p753 = _p752;
	return _elm_lang$lazy$Lazy$force(_elm_community$elm_time$Time_TimeZoneData$america_whitehorse_l);
};
var _elm_community$elm_time$Time_TimeZones$america_virgin = function (_p754) {
	var _p755 = _p754;
	return _elm_lang$lazy$Lazy$force(
		A2(_elm_community$elm_time$Time_TimeZoneData$link, 'America/Port_of_Spain', _elm_community$elm_time$Time_TimeZoneData$america_port_of_spain_l));
};
var _elm_community$elm_time$Time_TimeZones$america_vancouver = function (_p756) {
	var _p757 = _p756;
	return _elm_lang$lazy$Lazy$force(_elm_community$elm_time$Time_TimeZoneData$america_vancouver_l);
};
var _elm_community$elm_time$Time_TimeZones$america_tortola = function (_p758) {
	var _p759 = _p758;
	return _elm_lang$lazy$Lazy$force(
		A2(_elm_community$elm_time$Time_TimeZoneData$link, 'America/Port_of_Spain', _elm_community$elm_time$Time_TimeZoneData$america_port_of_spain_l));
};
var _elm_community$elm_time$Time_TimeZones$america_toronto = function (_p760) {
	var _p761 = _p760;
	return _elm_lang$lazy$Lazy$force(_elm_community$elm_time$Time_TimeZoneData$america_toronto_l);
};
var _elm_community$elm_time$Time_TimeZones$america_tijuana = function (_p762) {
	var _p763 = _p762;
	return _elm_lang$lazy$Lazy$force(_elm_community$elm_time$Time_TimeZoneData$america_tijuana_l);
};
var _elm_community$elm_time$Time_TimeZones$america_thunder_bay = function (_p764) {
	var _p765 = _p764;
	return _elm_lang$lazy$Lazy$force(_elm_community$elm_time$Time_TimeZoneData$america_thunder_bay_l);
};
var _elm_community$elm_time$Time_TimeZones$america_thule = function (_p766) {
	var _p767 = _p766;
	return _elm_lang$lazy$Lazy$force(_elm_community$elm_time$Time_TimeZoneData$america_thule_l);
};
var _elm_community$elm_time$Time_TimeZones$america_tegucigalpa = function (_p768) {
	var _p769 = _p768;
	return _elm_lang$lazy$Lazy$force(_elm_community$elm_time$Time_TimeZoneData$america_tegucigalpa_l);
};
var _elm_community$elm_time$Time_TimeZones$america_swift_current = function (_p770) {
	var _p771 = _p770;
	return _elm_lang$lazy$Lazy$force(_elm_community$elm_time$Time_TimeZoneData$america_swift_current_l);
};
var _elm_community$elm_time$Time_TimeZones$america_st_vincent = function (_p772) {
	var _p773 = _p772;
	return _elm_lang$lazy$Lazy$force(
		A2(_elm_community$elm_time$Time_TimeZoneData$link, 'America/Port_of_Spain', _elm_community$elm_time$Time_TimeZoneData$america_port_of_spain_l));
};
var _elm_community$elm_time$Time_TimeZones$america_st_thomas = function (_p774) {
	var _p775 = _p774;
	return _elm_lang$lazy$Lazy$force(
		A2(_elm_community$elm_time$Time_TimeZoneData$link, 'America/Port_of_Spain', _elm_community$elm_time$Time_TimeZoneData$america_port_of_spain_l));
};
var _elm_community$elm_time$Time_TimeZones$america_st_lucia = function (_p776) {
	var _p777 = _p776;
	return _elm_lang$lazy$Lazy$force(
		A2(_elm_community$elm_time$Time_TimeZoneData$link, 'America/Port_of_Spain', _elm_community$elm_time$Time_TimeZoneData$america_port_of_spain_l));
};
var _elm_community$elm_time$Time_TimeZones$america_st_kitts = function (_p778) {
	var _p779 = _p778;
	return _elm_lang$lazy$Lazy$force(
		A2(_elm_community$elm_time$Time_TimeZoneData$link, 'America/Port_of_Spain', _elm_community$elm_time$Time_TimeZoneData$america_port_of_spain_l));
};
var _elm_community$elm_time$Time_TimeZones$america_st_johns = function (_p780) {
	var _p781 = _p780;
	return _elm_lang$lazy$Lazy$force(_elm_community$elm_time$Time_TimeZoneData$america_st_johns_l);
};
var _elm_community$elm_time$Time_TimeZones$america_st_barthelemy = function (_p782) {
	var _p783 = _p782;
	return _elm_lang$lazy$Lazy$force(
		A2(_elm_community$elm_time$Time_TimeZoneData$link, 'America/Port_of_Spain', _elm_community$elm_time$Time_TimeZoneData$america_port_of_spain_l));
};
var _elm_community$elm_time$Time_TimeZones$america_sitka = function (_p784) {
	var _p785 = _p784;
	return _elm_lang$lazy$Lazy$force(_elm_community$elm_time$Time_TimeZoneData$america_sitka_l);
};
var _elm_community$elm_time$Time_TimeZones$america_shiprock = function (_p786) {
	var _p787 = _p786;
	return _elm_lang$lazy$Lazy$force(
		A2(_elm_community$elm_time$Time_TimeZoneData$link, 'America/Denver', _elm_community$elm_time$Time_TimeZoneData$america_denver_l));
};
var _elm_community$elm_time$Time_TimeZones$america_scoresbysund = function (_p788) {
	var _p789 = _p788;
	return _elm_lang$lazy$Lazy$force(_elm_community$elm_time$Time_TimeZoneData$america_scoresbysund_l);
};
var _elm_community$elm_time$Time_TimeZones$america_sao_paulo = function (_p790) {
	var _p791 = _p790;
	return _elm_lang$lazy$Lazy$force(_elm_community$elm_time$Time_TimeZoneData$america_sao_paulo_l);
};
var _elm_community$elm_time$Time_TimeZones$america_santo_domingo = function (_p792) {
	var _p793 = _p792;
	return _elm_lang$lazy$Lazy$force(_elm_community$elm_time$Time_TimeZoneData$america_santo_domingo_l);
};
var _elm_community$elm_time$Time_TimeZones$america_santiago = function (_p794) {
	var _p795 = _p794;
	return _elm_lang$lazy$Lazy$force(_elm_community$elm_time$Time_TimeZoneData$america_santiago_l);
};
var _elm_community$elm_time$Time_TimeZones$america_santarem = function (_p796) {
	var _p797 = _p796;
	return _elm_lang$lazy$Lazy$force(_elm_community$elm_time$Time_TimeZoneData$america_santarem_l);
};
var _elm_community$elm_time$Time_TimeZones$america_santa_isabel = function (_p798) {
	var _p799 = _p798;
	return _elm_lang$lazy$Lazy$force(
		A2(_elm_community$elm_time$Time_TimeZoneData$link, 'America/Tijuana', _elm_community$elm_time$Time_TimeZoneData$america_tijuana_l));
};
var _elm_community$elm_time$Time_TimeZones$america_rosario = function (_p800) {
	var _p801 = _p800;
	return _elm_lang$lazy$Lazy$force(
		A2(_elm_community$elm_time$Time_TimeZoneData$link, 'America/Argentina/Cordoba', _elm_community$elm_time$Time_TimeZoneData$america_argentina_cordoba_l));
};
var _elm_community$elm_time$Time_TimeZones$america_rio_branco = function (_p802) {
	var _p803 = _p802;
	return _elm_lang$lazy$Lazy$force(_elm_community$elm_time$Time_TimeZoneData$america_rio_branco_l);
};
var _elm_community$elm_time$Time_TimeZones$america_resolute = function (_p804) {
	var _p805 = _p804;
	return _elm_lang$lazy$Lazy$force(_elm_community$elm_time$Time_TimeZoneData$america_resolute_l);
};
var _elm_community$elm_time$Time_TimeZones$america_regina = function (_p806) {
	var _p807 = _p806;
	return _elm_lang$lazy$Lazy$force(_elm_community$elm_time$Time_TimeZoneData$america_regina_l);
};
var _elm_community$elm_time$Time_TimeZones$america_recife = function (_p808) {
	var _p809 = _p808;
	return _elm_lang$lazy$Lazy$force(_elm_community$elm_time$Time_TimeZoneData$america_recife_l);
};
var _elm_community$elm_time$Time_TimeZones$america_rankin_inlet = function (_p810) {
	var _p811 = _p810;
	return _elm_lang$lazy$Lazy$force(_elm_community$elm_time$Time_TimeZoneData$america_rankin_inlet_l);
};
var _elm_community$elm_time$Time_TimeZones$america_rainy_river = function (_p812) {
	var _p813 = _p812;
	return _elm_lang$lazy$Lazy$force(_elm_community$elm_time$Time_TimeZoneData$america_rainy_river_l);
};
var _elm_community$elm_time$Time_TimeZones$america_puerto_rico = function (_p814) {
	var _p815 = _p814;
	return _elm_lang$lazy$Lazy$force(_elm_community$elm_time$Time_TimeZoneData$america_puerto_rico_l);
};
var _elm_community$elm_time$Time_TimeZones$america_porto_velho = function (_p816) {
	var _p817 = _p816;
	return _elm_lang$lazy$Lazy$force(_elm_community$elm_time$Time_TimeZoneData$america_porto_velho_l);
};
var _elm_community$elm_time$Time_TimeZones$america_porto_acre = function (_p818) {
	var _p819 = _p818;
	return _elm_lang$lazy$Lazy$force(
		A2(_elm_community$elm_time$Time_TimeZoneData$link, 'America/Rio_Branco', _elm_community$elm_time$Time_TimeZoneData$america_rio_branco_l));
};
var _elm_community$elm_time$Time_TimeZones$america_port_of_spain = function (_p820) {
	var _p821 = _p820;
	return _elm_lang$lazy$Lazy$force(_elm_community$elm_time$Time_TimeZoneData$america_port_of_spain_l);
};
var _elm_community$elm_time$Time_TimeZones$america_port_au_prince = function (_p822) {
	var _p823 = _p822;
	return _elm_lang$lazy$Lazy$force(_elm_community$elm_time$Time_TimeZoneData$america_port_au_prince_l);
};
var _elm_community$elm_time$Time_TimeZones$america_phoenix = function (_p824) {
	var _p825 = _p824;
	return _elm_lang$lazy$Lazy$force(_elm_community$elm_time$Time_TimeZoneData$america_phoenix_l);
};
var _elm_community$elm_time$Time_TimeZones$america_paramaribo = function (_p826) {
	var _p827 = _p826;
	return _elm_lang$lazy$Lazy$force(_elm_community$elm_time$Time_TimeZoneData$america_paramaribo_l);
};
var _elm_community$elm_time$Time_TimeZones$america_pangnirtung = function (_p828) {
	var _p829 = _p828;
	return _elm_lang$lazy$Lazy$force(_elm_community$elm_time$Time_TimeZoneData$america_pangnirtung_l);
};
var _elm_community$elm_time$Time_TimeZones$america_panama = function (_p830) {
	var _p831 = _p830;
	return _elm_lang$lazy$Lazy$force(_elm_community$elm_time$Time_TimeZoneData$america_panama_l);
};
var _elm_community$elm_time$Time_TimeZones$america_ojinaga = function (_p832) {
	var _p833 = _p832;
	return _elm_lang$lazy$Lazy$force(_elm_community$elm_time$Time_TimeZoneData$america_ojinaga_l);
};
var _elm_community$elm_time$Time_TimeZones$america_north_dakota_new_salem = function (_p834) {
	var _p835 = _p834;
	return _elm_lang$lazy$Lazy$force(_elm_community$elm_time$Time_TimeZoneData$america_north_dakota_new_salem_l);
};
var _elm_community$elm_time$Time_TimeZones$america_north_dakota_center = function (_p836) {
	var _p837 = _p836;
	return _elm_lang$lazy$Lazy$force(_elm_community$elm_time$Time_TimeZoneData$america_north_dakota_center_l);
};
var _elm_community$elm_time$Time_TimeZones$america_north_dakota_beulah = function (_p838) {
	var _p839 = _p838;
	return _elm_lang$lazy$Lazy$force(_elm_community$elm_time$Time_TimeZoneData$america_north_dakota_beulah_l);
};
var _elm_community$elm_time$Time_TimeZones$america_noronha = function (_p840) {
	var _p841 = _p840;
	return _elm_lang$lazy$Lazy$force(_elm_community$elm_time$Time_TimeZoneData$america_noronha_l);
};
var _elm_community$elm_time$Time_TimeZones$america_nome = function (_p842) {
	var _p843 = _p842;
	return _elm_lang$lazy$Lazy$force(_elm_community$elm_time$Time_TimeZoneData$america_nome_l);
};
var _elm_community$elm_time$Time_TimeZones$america_nipigon = function (_p844) {
	var _p845 = _p844;
	return _elm_lang$lazy$Lazy$force(_elm_community$elm_time$Time_TimeZoneData$america_nipigon_l);
};
var _elm_community$elm_time$Time_TimeZones$america_new_york = function (_p846) {
	var _p847 = _p846;
	return _elm_lang$lazy$Lazy$force(_elm_community$elm_time$Time_TimeZoneData$america_new_york_l);
};
var _elm_community$elm_time$Time_TimeZones$america_nassau = function (_p848) {
	var _p849 = _p848;
	return _elm_lang$lazy$Lazy$force(_elm_community$elm_time$Time_TimeZoneData$america_nassau_l);
};
var _elm_community$elm_time$Time_TimeZones$america_montserrat = function (_p850) {
	var _p851 = _p850;
	return _elm_lang$lazy$Lazy$force(
		A2(_elm_community$elm_time$Time_TimeZoneData$link, 'America/Port_of_Spain', _elm_community$elm_time$Time_TimeZoneData$america_port_of_spain_l));
};
var _elm_community$elm_time$Time_TimeZones$america_montreal = function (_p852) {
	var _p853 = _p852;
	return _elm_lang$lazy$Lazy$force(
		A2(_elm_community$elm_time$Time_TimeZoneData$link, 'America/Toronto', _elm_community$elm_time$Time_TimeZoneData$america_toronto_l));
};
var _elm_community$elm_time$Time_TimeZones$america_montevideo = function (_p854) {
	var _p855 = _p854;
	return _elm_lang$lazy$Lazy$force(_elm_community$elm_time$Time_TimeZoneData$america_montevideo_l);
};
var _elm_community$elm_time$Time_TimeZones$america_monterrey = function (_p856) {
	var _p857 = _p856;
	return _elm_lang$lazy$Lazy$force(_elm_community$elm_time$Time_TimeZoneData$america_monterrey_l);
};
var _elm_community$elm_time$Time_TimeZones$america_moncton = function (_p858) {
	var _p859 = _p858;
	return _elm_lang$lazy$Lazy$force(_elm_community$elm_time$Time_TimeZoneData$america_moncton_l);
};
var _elm_community$elm_time$Time_TimeZones$america_miquelon = function (_p860) {
	var _p861 = _p860;
	return _elm_lang$lazy$Lazy$force(_elm_community$elm_time$Time_TimeZoneData$america_miquelon_l);
};
var _elm_community$elm_time$Time_TimeZones$america_mexico_city = function (_p862) {
	var _p863 = _p862;
	return _elm_lang$lazy$Lazy$force(_elm_community$elm_time$Time_TimeZoneData$america_mexico_city_l);
};
var _elm_community$elm_time$Time_TimeZones$america_metlakatla = function (_p864) {
	var _p865 = _p864;
	return _elm_lang$lazy$Lazy$force(_elm_community$elm_time$Time_TimeZoneData$america_metlakatla_l);
};
var _elm_community$elm_time$Time_TimeZones$america_merida = function (_p866) {
	var _p867 = _p866;
	return _elm_lang$lazy$Lazy$force(_elm_community$elm_time$Time_TimeZoneData$america_merida_l);
};
var _elm_community$elm_time$Time_TimeZones$america_menominee = function (_p868) {
	var _p869 = _p868;
	return _elm_lang$lazy$Lazy$force(_elm_community$elm_time$Time_TimeZoneData$america_menominee_l);
};
var _elm_community$elm_time$Time_TimeZones$america_mendoza = function (_p870) {
	var _p871 = _p870;
	return _elm_lang$lazy$Lazy$force(
		A2(_elm_community$elm_time$Time_TimeZoneData$link, 'America/Argentina/Mendoza', _elm_community$elm_time$Time_TimeZoneData$america_argentina_mendoza_l));
};
var _elm_community$elm_time$Time_TimeZones$america_mazatlan = function (_p872) {
	var _p873 = _p872;
	return _elm_lang$lazy$Lazy$force(_elm_community$elm_time$Time_TimeZoneData$america_mazatlan_l);
};
var _elm_community$elm_time$Time_TimeZones$america_matamoros = function (_p874) {
	var _p875 = _p874;
	return _elm_lang$lazy$Lazy$force(_elm_community$elm_time$Time_TimeZoneData$america_matamoros_l);
};
var _elm_community$elm_time$Time_TimeZones$america_martinique = function (_p876) {
	var _p877 = _p876;
	return _elm_lang$lazy$Lazy$force(_elm_community$elm_time$Time_TimeZoneData$america_martinique_l);
};
var _elm_community$elm_time$Time_TimeZones$america_marigot = function (_p878) {
	var _p879 = _p878;
	return _elm_lang$lazy$Lazy$force(
		A2(_elm_community$elm_time$Time_TimeZoneData$link, 'America/Port_of_Spain', _elm_community$elm_time$Time_TimeZoneData$america_port_of_spain_l));
};
var _elm_community$elm_time$Time_TimeZones$america_manaus = function (_p880) {
	var _p881 = _p880;
	return _elm_lang$lazy$Lazy$force(_elm_community$elm_time$Time_TimeZoneData$america_manaus_l);
};
var _elm_community$elm_time$Time_TimeZones$america_managua = function (_p882) {
	var _p883 = _p882;
	return _elm_lang$lazy$Lazy$force(_elm_community$elm_time$Time_TimeZoneData$america_managua_l);
};
var _elm_community$elm_time$Time_TimeZones$america_maceio = function (_p884) {
	var _p885 = _p884;
	return _elm_lang$lazy$Lazy$force(_elm_community$elm_time$Time_TimeZoneData$america_maceio_l);
};
var _elm_community$elm_time$Time_TimeZones$america_lower_princes = function (_p886) {
	var _p887 = _p886;
	return _elm_lang$lazy$Lazy$force(
		A2(_elm_community$elm_time$Time_TimeZoneData$link, 'America/Curacao', _elm_community$elm_time$Time_TimeZoneData$america_curacao_l));
};
var _elm_community$elm_time$Time_TimeZones$america_louisville = function (_p888) {
	var _p889 = _p888;
	return _elm_lang$lazy$Lazy$force(
		A2(_elm_community$elm_time$Time_TimeZoneData$link, 'America/Kentucky/Louisville', _elm_community$elm_time$Time_TimeZoneData$america_kentucky_louisville_l));
};
var _elm_community$elm_time$Time_TimeZones$america_los_angeles = function (_p890) {
	var _p891 = _p890;
	return _elm_lang$lazy$Lazy$force(_elm_community$elm_time$Time_TimeZoneData$america_los_angeles_l);
};
var _elm_community$elm_time$Time_TimeZones$america_lima = function (_p892) {
	var _p893 = _p892;
	return _elm_lang$lazy$Lazy$force(_elm_community$elm_time$Time_TimeZoneData$america_lima_l);
};
var _elm_community$elm_time$Time_TimeZones$america_la_paz = function (_p894) {
	var _p895 = _p894;
	return _elm_lang$lazy$Lazy$force(_elm_community$elm_time$Time_TimeZoneData$america_la_paz_l);
};
var _elm_community$elm_time$Time_TimeZones$america_kralendijk = function (_p896) {
	var _p897 = _p896;
	return _elm_lang$lazy$Lazy$force(
		A2(_elm_community$elm_time$Time_TimeZoneData$link, 'America/Curacao', _elm_community$elm_time$Time_TimeZoneData$america_curacao_l));
};
var _elm_community$elm_time$Time_TimeZones$america_knox_in = function (_p898) {
	var _p899 = _p898;
	return _elm_lang$lazy$Lazy$force(
		A2(_elm_community$elm_time$Time_TimeZoneData$link, 'America/Indiana/Knox', _elm_community$elm_time$Time_TimeZoneData$america_indiana_knox_l));
};
var _elm_community$elm_time$Time_TimeZones$america_kentucky_monticello = function (_p900) {
	var _p901 = _p900;
	return _elm_lang$lazy$Lazy$force(_elm_community$elm_time$Time_TimeZoneData$america_kentucky_monticello_l);
};
var _elm_community$elm_time$Time_TimeZones$america_kentucky_louisville = function (_p902) {
	var _p903 = _p902;
	return _elm_lang$lazy$Lazy$force(_elm_community$elm_time$Time_TimeZoneData$america_kentucky_louisville_l);
};
var _elm_community$elm_time$Time_TimeZones$america_juneau = function (_p904) {
	var _p905 = _p904;
	return _elm_lang$lazy$Lazy$force(_elm_community$elm_time$Time_TimeZoneData$america_juneau_l);
};
var _elm_community$elm_time$Time_TimeZones$america_jujuy = function (_p906) {
	var _p907 = _p906;
	return _elm_lang$lazy$Lazy$force(
		A2(_elm_community$elm_time$Time_TimeZoneData$link, 'America/Argentina/Jujuy', _elm_community$elm_time$Time_TimeZoneData$america_argentina_jujuy_l));
};
var _elm_community$elm_time$Time_TimeZones$america_jamaica = function (_p908) {
	var _p909 = _p908;
	return _elm_lang$lazy$Lazy$force(_elm_community$elm_time$Time_TimeZoneData$america_jamaica_l);
};
var _elm_community$elm_time$Time_TimeZones$america_iqaluit = function (_p910) {
	var _p911 = _p910;
	return _elm_lang$lazy$Lazy$force(_elm_community$elm_time$Time_TimeZoneData$america_iqaluit_l);
};
var _elm_community$elm_time$Time_TimeZones$america_inuvik = function (_p912) {
	var _p913 = _p912;
	return _elm_lang$lazy$Lazy$force(_elm_community$elm_time$Time_TimeZoneData$america_inuvik_l);
};
var _elm_community$elm_time$Time_TimeZones$america_indianapolis = function (_p914) {
	var _p915 = _p914;
	return _elm_lang$lazy$Lazy$force(
		A2(_elm_community$elm_time$Time_TimeZoneData$link, 'America/Fort_Wayne', _elm_community$elm_time$Time_TimeZoneData$america_fort_wayne_l));
};
var _elm_community$elm_time$Time_TimeZones$america_indiana_winamac = function (_p916) {
	var _p917 = _p916;
	return _elm_lang$lazy$Lazy$force(_elm_community$elm_time$Time_TimeZoneData$america_indiana_winamac_l);
};
var _elm_community$elm_time$Time_TimeZones$america_indiana_vincennes = function (_p918) {
	var _p919 = _p918;
	return _elm_lang$lazy$Lazy$force(_elm_community$elm_time$Time_TimeZoneData$america_indiana_vincennes_l);
};
var _elm_community$elm_time$Time_TimeZones$america_indiana_vevay = function (_p920) {
	var _p921 = _p920;
	return _elm_lang$lazy$Lazy$force(_elm_community$elm_time$Time_TimeZoneData$america_indiana_vevay_l);
};
var _elm_community$elm_time$Time_TimeZones$america_indiana_tell_city = function (_p922) {
	var _p923 = _p922;
	return _elm_lang$lazy$Lazy$force(_elm_community$elm_time$Time_TimeZoneData$america_indiana_tell_city_l);
};
var _elm_community$elm_time$Time_TimeZones$america_indiana_petersburg = function (_p924) {
	var _p925 = _p924;
	return _elm_lang$lazy$Lazy$force(_elm_community$elm_time$Time_TimeZoneData$america_indiana_petersburg_l);
};
var _elm_community$elm_time$Time_TimeZones$america_indiana_marengo = function (_p926) {
	var _p927 = _p926;
	return _elm_lang$lazy$Lazy$force(_elm_community$elm_time$Time_TimeZoneData$america_indiana_marengo_l);
};
var _elm_community$elm_time$Time_TimeZones$america_indiana_knox = function (_p928) {
	var _p929 = _p928;
	return _elm_lang$lazy$Lazy$force(_elm_community$elm_time$Time_TimeZoneData$america_indiana_knox_l);
};
var _elm_community$elm_time$Time_TimeZones$america_indiana_indianapolis = function (_p930) {
	var _p931 = _p930;
	return _elm_lang$lazy$Lazy$force(
		A2(_elm_community$elm_time$Time_TimeZoneData$link, 'America/Fort_Wayne', _elm_community$elm_time$Time_TimeZoneData$america_fort_wayne_l));
};
var _elm_community$elm_time$Time_TimeZones$america_hermosillo = function (_p932) {
	var _p933 = _p932;
	return _elm_lang$lazy$Lazy$force(_elm_community$elm_time$Time_TimeZoneData$america_hermosillo_l);
};
var _elm_community$elm_time$Time_TimeZones$america_havana = function (_p934) {
	var _p935 = _p934;
	return _elm_lang$lazy$Lazy$force(_elm_community$elm_time$Time_TimeZoneData$america_havana_l);
};
var _elm_community$elm_time$Time_TimeZones$america_halifax = function (_p936) {
	var _p937 = _p936;
	return _elm_lang$lazy$Lazy$force(_elm_community$elm_time$Time_TimeZoneData$america_halifax_l);
};
var _elm_community$elm_time$Time_TimeZones$america_guyana = function (_p938) {
	var _p939 = _p938;
	return _elm_lang$lazy$Lazy$force(_elm_community$elm_time$Time_TimeZoneData$america_guyana_l);
};
var _elm_community$elm_time$Time_TimeZones$america_guayaquil = function (_p940) {
	var _p941 = _p940;
	return _elm_lang$lazy$Lazy$force(_elm_community$elm_time$Time_TimeZoneData$america_guayaquil_l);
};
var _elm_community$elm_time$Time_TimeZones$america_guatemala = function (_p942) {
	var _p943 = _p942;
	return _elm_lang$lazy$Lazy$force(_elm_community$elm_time$Time_TimeZoneData$america_guatemala_l);
};
var _elm_community$elm_time$Time_TimeZones$america_guadeloupe = function (_p944) {
	var _p945 = _p944;
	return _elm_lang$lazy$Lazy$force(
		A2(_elm_community$elm_time$Time_TimeZoneData$link, 'America/Port_of_Spain', _elm_community$elm_time$Time_TimeZoneData$america_port_of_spain_l));
};
var _elm_community$elm_time$Time_TimeZones$america_grenada = function (_p946) {
	var _p947 = _p946;
	return _elm_lang$lazy$Lazy$force(
		A2(_elm_community$elm_time$Time_TimeZoneData$link, 'America/Port_of_Spain', _elm_community$elm_time$Time_TimeZoneData$america_port_of_spain_l));
};
var _elm_community$elm_time$Time_TimeZones$america_grand_turk = function (_p948) {
	var _p949 = _p948;
	return _elm_lang$lazy$Lazy$force(_elm_community$elm_time$Time_TimeZoneData$america_grand_turk_l);
};
var _elm_community$elm_time$Time_TimeZones$america_goose_bay = function (_p950) {
	var _p951 = _p950;
	return _elm_lang$lazy$Lazy$force(_elm_community$elm_time$Time_TimeZoneData$america_goose_bay_l);
};
var _elm_community$elm_time$Time_TimeZones$america_godthab = function (_p952) {
	var _p953 = _p952;
	return _elm_lang$lazy$Lazy$force(_elm_community$elm_time$Time_TimeZoneData$america_godthab_l);
};
var _elm_community$elm_time$Time_TimeZones$america_glace_bay = function (_p954) {
	var _p955 = _p954;
	return _elm_lang$lazy$Lazy$force(_elm_community$elm_time$Time_TimeZoneData$america_glace_bay_l);
};
var _elm_community$elm_time$Time_TimeZones$america_fortaleza = function (_p956) {
	var _p957 = _p956;
	return _elm_lang$lazy$Lazy$force(_elm_community$elm_time$Time_TimeZoneData$america_fortaleza_l);
};
var _elm_community$elm_time$Time_TimeZones$america_fort_wayne = function (_p958) {
	var _p959 = _p958;
	return _elm_lang$lazy$Lazy$force(_elm_community$elm_time$Time_TimeZoneData$america_fort_wayne_l);
};
var _elm_community$elm_time$Time_TimeZones$america_fort_nelson = function (_p960) {
	var _p961 = _p960;
	return _elm_lang$lazy$Lazy$force(_elm_community$elm_time$Time_TimeZoneData$america_fort_nelson_l);
};
var _elm_community$elm_time$Time_TimeZones$america_ensenada = function (_p962) {
	var _p963 = _p962;
	return _elm_lang$lazy$Lazy$force(
		A2(_elm_community$elm_time$Time_TimeZoneData$link, 'America/Tijuana', _elm_community$elm_time$Time_TimeZoneData$america_tijuana_l));
};
var _elm_community$elm_time$Time_TimeZones$america_el_salvador = function (_p964) {
	var _p965 = _p964;
	return _elm_lang$lazy$Lazy$force(_elm_community$elm_time$Time_TimeZoneData$america_el_salvador_l);
};
var _elm_community$elm_time$Time_TimeZones$america_eirunepe = function (_p966) {
	var _p967 = _p966;
	return _elm_lang$lazy$Lazy$force(_elm_community$elm_time$Time_TimeZoneData$america_eirunepe_l);
};
var _elm_community$elm_time$Time_TimeZones$america_edmonton = function (_p968) {
	var _p969 = _p968;
	return _elm_lang$lazy$Lazy$force(_elm_community$elm_time$Time_TimeZoneData$america_edmonton_l);
};
var _elm_community$elm_time$Time_TimeZones$america_dominica = function (_p970) {
	var _p971 = _p970;
	return _elm_lang$lazy$Lazy$force(
		A2(_elm_community$elm_time$Time_TimeZoneData$link, 'America/Port_of_Spain', _elm_community$elm_time$Time_TimeZoneData$america_port_of_spain_l));
};
var _elm_community$elm_time$Time_TimeZones$america_detroit = function (_p972) {
	var _p973 = _p972;
	return _elm_lang$lazy$Lazy$force(_elm_community$elm_time$Time_TimeZoneData$america_detroit_l);
};
var _elm_community$elm_time$Time_TimeZones$america_denver = function (_p974) {
	var _p975 = _p974;
	return _elm_lang$lazy$Lazy$force(_elm_community$elm_time$Time_TimeZoneData$america_denver_l);
};
var _elm_community$elm_time$Time_TimeZones$america_dawson_creek = function (_p976) {
	var _p977 = _p976;
	return _elm_lang$lazy$Lazy$force(_elm_community$elm_time$Time_TimeZoneData$america_dawson_creek_l);
};
var _elm_community$elm_time$Time_TimeZones$america_dawson = function (_p978) {
	var _p979 = _p978;
	return _elm_lang$lazy$Lazy$force(_elm_community$elm_time$Time_TimeZoneData$america_dawson_l);
};
var _elm_community$elm_time$Time_TimeZones$america_danmarkshavn = function (_p980) {
	var _p981 = _p980;
	return _elm_lang$lazy$Lazy$force(_elm_community$elm_time$Time_TimeZoneData$america_danmarkshavn_l);
};
var _elm_community$elm_time$Time_TimeZones$america_curacao = function (_p982) {
	var _p983 = _p982;
	return _elm_lang$lazy$Lazy$force(_elm_community$elm_time$Time_TimeZoneData$america_curacao_l);
};
var _elm_community$elm_time$Time_TimeZones$america_cuiaba = function (_p984) {
	var _p985 = _p984;
	return _elm_lang$lazy$Lazy$force(_elm_community$elm_time$Time_TimeZoneData$america_cuiaba_l);
};
var _elm_community$elm_time$Time_TimeZones$america_creston = function (_p986) {
	var _p987 = _p986;
	return _elm_lang$lazy$Lazy$force(_elm_community$elm_time$Time_TimeZoneData$america_creston_l);
};
var _elm_community$elm_time$Time_TimeZones$america_costa_rica = function (_p988) {
	var _p989 = _p988;
	return _elm_lang$lazy$Lazy$force(_elm_community$elm_time$Time_TimeZoneData$america_costa_rica_l);
};
var _elm_community$elm_time$Time_TimeZones$america_cordoba = function (_p990) {
	var _p991 = _p990;
	return _elm_lang$lazy$Lazy$force(
		A2(_elm_community$elm_time$Time_TimeZoneData$link, 'America/Argentina/Cordoba', _elm_community$elm_time$Time_TimeZoneData$america_argentina_cordoba_l));
};
var _elm_community$elm_time$Time_TimeZones$america_coral_harbour = function (_p992) {
	var _p993 = _p992;
	return _elm_lang$lazy$Lazy$force(
		A2(_elm_community$elm_time$Time_TimeZoneData$link, 'America/Atikokan', _elm_community$elm_time$Time_TimeZoneData$america_atikokan_l));
};
var _elm_community$elm_time$Time_TimeZones$america_chihuahua = function (_p994) {
	var _p995 = _p994;
	return _elm_lang$lazy$Lazy$force(_elm_community$elm_time$Time_TimeZoneData$america_chihuahua_l);
};
var _elm_community$elm_time$Time_TimeZones$america_chicago = function (_p996) {
	var _p997 = _p996;
	return _elm_lang$lazy$Lazy$force(_elm_community$elm_time$Time_TimeZoneData$america_chicago_l);
};
var _elm_community$elm_time$Time_TimeZones$america_cayman = function (_p998) {
	var _p999 = _p998;
	return _elm_lang$lazy$Lazy$force(
		A2(_elm_community$elm_time$Time_TimeZoneData$link, 'America/Panama', _elm_community$elm_time$Time_TimeZoneData$america_panama_l));
};
var _elm_community$elm_time$Time_TimeZones$america_cayenne = function (_p1000) {
	var _p1001 = _p1000;
	return _elm_lang$lazy$Lazy$force(_elm_community$elm_time$Time_TimeZoneData$america_cayenne_l);
};
var _elm_community$elm_time$Time_TimeZones$america_catamarca = function (_p1002) {
	var _p1003 = _p1002;
	return _elm_lang$lazy$Lazy$force(
		A2(_elm_community$elm_time$Time_TimeZoneData$link, 'America/Argentina/Catamarca', _elm_community$elm_time$Time_TimeZoneData$america_argentina_catamarca_l));
};
var _elm_community$elm_time$Time_TimeZones$america_caracas = function (_p1004) {
	var _p1005 = _p1004;
	return _elm_lang$lazy$Lazy$force(_elm_community$elm_time$Time_TimeZoneData$america_caracas_l);
};
var _elm_community$elm_time$Time_TimeZones$america_cancun = function (_p1006) {
	var _p1007 = _p1006;
	return _elm_lang$lazy$Lazy$force(_elm_community$elm_time$Time_TimeZoneData$america_cancun_l);
};
var _elm_community$elm_time$Time_TimeZones$america_campo_grande = function (_p1008) {
	var _p1009 = _p1008;
	return _elm_lang$lazy$Lazy$force(_elm_community$elm_time$Time_TimeZoneData$america_campo_grande_l);
};
var _elm_community$elm_time$Time_TimeZones$america_cambridge_bay = function (_p1010) {
	var _p1011 = _p1010;
	return _elm_lang$lazy$Lazy$force(_elm_community$elm_time$Time_TimeZoneData$america_cambridge_bay_l);
};
var _elm_community$elm_time$Time_TimeZones$america_buenos_aires = function (_p1012) {
	var _p1013 = _p1012;
	return _elm_lang$lazy$Lazy$force(
		A2(_elm_community$elm_time$Time_TimeZoneData$link, 'America/Argentina/Buenos_Aires', _elm_community$elm_time$Time_TimeZoneData$america_argentina_buenos_aires_l));
};
var _elm_community$elm_time$Time_TimeZones$america_boise = function (_p1014) {
	var _p1015 = _p1014;
	return _elm_lang$lazy$Lazy$force(_elm_community$elm_time$Time_TimeZoneData$america_boise_l);
};
var _elm_community$elm_time$Time_TimeZones$america_bogota = function (_p1016) {
	var _p1017 = _p1016;
	return _elm_lang$lazy$Lazy$force(_elm_community$elm_time$Time_TimeZoneData$america_bogota_l);
};
var _elm_community$elm_time$Time_TimeZones$america_boa_vista = function (_p1018) {
	var _p1019 = _p1018;
	return _elm_lang$lazy$Lazy$force(_elm_community$elm_time$Time_TimeZoneData$america_boa_vista_l);
};
var _elm_community$elm_time$Time_TimeZones$america_blanc_sablon = function (_p1020) {
	var _p1021 = _p1020;
	return _elm_lang$lazy$Lazy$force(_elm_community$elm_time$Time_TimeZoneData$america_blanc_sablon_l);
};
var _elm_community$elm_time$Time_TimeZones$america_belize = function (_p1022) {
	var _p1023 = _p1022;
	return _elm_lang$lazy$Lazy$force(_elm_community$elm_time$Time_TimeZoneData$america_belize_l);
};
var _elm_community$elm_time$Time_TimeZones$america_belem = function (_p1024) {
	var _p1025 = _p1024;
	return _elm_lang$lazy$Lazy$force(_elm_community$elm_time$Time_TimeZoneData$america_belem_l);
};
var _elm_community$elm_time$Time_TimeZones$america_barbados = function (_p1026) {
	var _p1027 = _p1026;
	return _elm_lang$lazy$Lazy$force(_elm_community$elm_time$Time_TimeZoneData$america_barbados_l);
};
var _elm_community$elm_time$Time_TimeZones$america_bahia_banderas = function (_p1028) {
	var _p1029 = _p1028;
	return _elm_lang$lazy$Lazy$force(_elm_community$elm_time$Time_TimeZoneData$america_bahia_banderas_l);
};
var _elm_community$elm_time$Time_TimeZones$america_bahia = function (_p1030) {
	var _p1031 = _p1030;
	return _elm_lang$lazy$Lazy$force(_elm_community$elm_time$Time_TimeZoneData$america_bahia_l);
};
var _elm_community$elm_time$Time_TimeZones$america_atka = function (_p1032) {
	var _p1033 = _p1032;
	return _elm_lang$lazy$Lazy$force(
		A2(_elm_community$elm_time$Time_TimeZoneData$link, 'America/Adak', _elm_community$elm_time$Time_TimeZoneData$america_adak_l));
};
var _elm_community$elm_time$Time_TimeZones$america_atikokan = function (_p1034) {
	var _p1035 = _p1034;
	return _elm_lang$lazy$Lazy$force(_elm_community$elm_time$Time_TimeZoneData$america_atikokan_l);
};
var _elm_community$elm_time$Time_TimeZones$america_asuncion = function (_p1036) {
	var _p1037 = _p1036;
	return _elm_lang$lazy$Lazy$force(_elm_community$elm_time$Time_TimeZoneData$america_asuncion_l);
};
var _elm_community$elm_time$Time_TimeZones$america_aruba = function (_p1038) {
	var _p1039 = _p1038;
	return _elm_lang$lazy$Lazy$force(
		A2(_elm_community$elm_time$Time_TimeZoneData$link, 'America/Curacao', _elm_community$elm_time$Time_TimeZoneData$america_curacao_l));
};
var _elm_community$elm_time$Time_TimeZones$america_argentina_ushuaia = function (_p1040) {
	var _p1041 = _p1040;
	return _elm_lang$lazy$Lazy$force(_elm_community$elm_time$Time_TimeZoneData$america_argentina_ushuaia_l);
};
var _elm_community$elm_time$Time_TimeZones$america_argentina_tucuman = function (_p1042) {
	var _p1043 = _p1042;
	return _elm_lang$lazy$Lazy$force(_elm_community$elm_time$Time_TimeZoneData$america_argentina_tucuman_l);
};
var _elm_community$elm_time$Time_TimeZones$america_argentina_san_luis = function (_p1044) {
	var _p1045 = _p1044;
	return _elm_lang$lazy$Lazy$force(_elm_community$elm_time$Time_TimeZoneData$america_argentina_san_luis_l);
};
var _elm_community$elm_time$Time_TimeZones$america_argentina_san_juan = function (_p1046) {
	var _p1047 = _p1046;
	return _elm_lang$lazy$Lazy$force(_elm_community$elm_time$Time_TimeZoneData$america_argentina_san_juan_l);
};
var _elm_community$elm_time$Time_TimeZones$america_argentina_salta = function (_p1048) {
	var _p1049 = _p1048;
	return _elm_lang$lazy$Lazy$force(_elm_community$elm_time$Time_TimeZoneData$america_argentina_salta_l);
};
var _elm_community$elm_time$Time_TimeZones$america_argentina_rio_gallegos = function (_p1050) {
	var _p1051 = _p1050;
	return _elm_lang$lazy$Lazy$force(_elm_community$elm_time$Time_TimeZoneData$america_argentina_rio_gallegos_l);
};
var _elm_community$elm_time$Time_TimeZones$america_argentina_mendoza = function (_p1052) {
	var _p1053 = _p1052;
	return _elm_lang$lazy$Lazy$force(_elm_community$elm_time$Time_TimeZoneData$america_argentina_mendoza_l);
};
var _elm_community$elm_time$Time_TimeZones$america_argentina_la_rioja = function (_p1054) {
	var _p1055 = _p1054;
	return _elm_lang$lazy$Lazy$force(_elm_community$elm_time$Time_TimeZoneData$america_argentina_la_rioja_l);
};
var _elm_community$elm_time$Time_TimeZones$america_argentina_jujuy = function (_p1056) {
	var _p1057 = _p1056;
	return _elm_lang$lazy$Lazy$force(_elm_community$elm_time$Time_TimeZoneData$america_argentina_jujuy_l);
};
var _elm_community$elm_time$Time_TimeZones$america_argentina_cordoba = function (_p1058) {
	var _p1059 = _p1058;
	return _elm_lang$lazy$Lazy$force(_elm_community$elm_time$Time_TimeZoneData$america_argentina_cordoba_l);
};
var _elm_community$elm_time$Time_TimeZones$america_argentina_comodrivadavia = function (_p1060) {
	var _p1061 = _p1060;
	return _elm_lang$lazy$Lazy$force(
		A2(_elm_community$elm_time$Time_TimeZoneData$link, 'America/Argentina/Catamarca', _elm_community$elm_time$Time_TimeZoneData$america_argentina_catamarca_l));
};
var _elm_community$elm_time$Time_TimeZones$america_argentina_catamarca = function (_p1062) {
	var _p1063 = _p1062;
	return _elm_lang$lazy$Lazy$force(_elm_community$elm_time$Time_TimeZoneData$america_argentina_catamarca_l);
};
var _elm_community$elm_time$Time_TimeZones$america_argentina_buenos_aires = function (_p1064) {
	var _p1065 = _p1064;
	return _elm_lang$lazy$Lazy$force(_elm_community$elm_time$Time_TimeZoneData$america_argentina_buenos_aires_l);
};
var _elm_community$elm_time$Time_TimeZones$america_araguaina = function (_p1066) {
	var _p1067 = _p1066;
	return _elm_lang$lazy$Lazy$force(_elm_community$elm_time$Time_TimeZoneData$america_araguaina_l);
};
var _elm_community$elm_time$Time_TimeZones$america_antigua = function (_p1068) {
	var _p1069 = _p1068;
	return _elm_lang$lazy$Lazy$force(
		A2(_elm_community$elm_time$Time_TimeZoneData$link, 'America/Port_of_Spain', _elm_community$elm_time$Time_TimeZoneData$america_port_of_spain_l));
};
var _elm_community$elm_time$Time_TimeZones$america_anguilla = function (_p1070) {
	var _p1071 = _p1070;
	return _elm_lang$lazy$Lazy$force(
		A2(_elm_community$elm_time$Time_TimeZoneData$link, 'America/Port_of_Spain', _elm_community$elm_time$Time_TimeZoneData$america_port_of_spain_l));
};
var _elm_community$elm_time$Time_TimeZones$america_anchorage = function (_p1072) {
	var _p1073 = _p1072;
	return _elm_lang$lazy$Lazy$force(_elm_community$elm_time$Time_TimeZoneData$america_anchorage_l);
};
var _elm_community$elm_time$Time_TimeZones$america_adak = function (_p1074) {
	var _p1075 = _p1074;
	return _elm_lang$lazy$Lazy$force(_elm_community$elm_time$Time_TimeZoneData$america_adak_l);
};
var _elm_community$elm_time$Time_TimeZones$africa_windhoek = function (_p1076) {
	var _p1077 = _p1076;
	return _elm_lang$lazy$Lazy$force(_elm_community$elm_time$Time_TimeZoneData$africa_windhoek_l);
};
var _elm_community$elm_time$Time_TimeZones$africa_tunis = function (_p1078) {
	var _p1079 = _p1078;
	return _elm_lang$lazy$Lazy$force(_elm_community$elm_time$Time_TimeZoneData$africa_tunis_l);
};
var _elm_community$elm_time$Time_TimeZones$africa_tripoli = function (_p1080) {
	var _p1081 = _p1080;
	return _elm_lang$lazy$Lazy$force(_elm_community$elm_time$Time_TimeZoneData$africa_tripoli_l);
};
var _elm_community$elm_time$Time_TimeZones$africa_timbuktu = function (_p1082) {
	var _p1083 = _p1082;
	return _elm_lang$lazy$Lazy$force(
		A2(_elm_community$elm_time$Time_TimeZoneData$link, 'Africa/Abidjan', _elm_community$elm_time$Time_TimeZoneData$africa_abidjan_l));
};
var _elm_community$elm_time$Time_TimeZones$africa_sao_tome = function (_p1084) {
	var _p1085 = _p1084;
	return _elm_lang$lazy$Lazy$force(
		A2(_elm_community$elm_time$Time_TimeZoneData$link, 'Africa/Abidjan', _elm_community$elm_time$Time_TimeZoneData$africa_abidjan_l));
};
var _elm_community$elm_time$Time_TimeZones$africa_porto_novo = function (_p1086) {
	var _p1087 = _p1086;
	return _elm_lang$lazy$Lazy$force(
		A2(_elm_community$elm_time$Time_TimeZoneData$link, 'Africa/Lagos', _elm_community$elm_time$Time_TimeZoneData$africa_lagos_l));
};
var _elm_community$elm_time$Time_TimeZones$africa_ouagadougou = function (_p1088) {
	var _p1089 = _p1088;
	return _elm_lang$lazy$Lazy$force(
		A2(_elm_community$elm_time$Time_TimeZoneData$link, 'Africa/Abidjan', _elm_community$elm_time$Time_TimeZoneData$africa_abidjan_l));
};
var _elm_community$elm_time$Time_TimeZones$africa_nouakchott = function (_p1090) {
	var _p1091 = _p1090;
	return _elm_lang$lazy$Lazy$force(
		A2(_elm_community$elm_time$Time_TimeZoneData$link, 'Africa/Abidjan', _elm_community$elm_time$Time_TimeZoneData$africa_abidjan_l));
};
var _elm_community$elm_time$Time_TimeZones$africa_niamey = function (_p1092) {
	var _p1093 = _p1092;
	return _elm_lang$lazy$Lazy$force(
		A2(_elm_community$elm_time$Time_TimeZoneData$link, 'Africa/Lagos', _elm_community$elm_time$Time_TimeZoneData$africa_lagos_l));
};
var _elm_community$elm_time$Time_TimeZones$africa_ndjamena = function (_p1094) {
	var _p1095 = _p1094;
	return _elm_lang$lazy$Lazy$force(_elm_community$elm_time$Time_TimeZoneData$africa_ndjamena_l);
};
var _elm_community$elm_time$Time_TimeZones$africa_nairobi = function (_p1096) {
	var _p1097 = _p1096;
	return _elm_lang$lazy$Lazy$force(_elm_community$elm_time$Time_TimeZoneData$africa_nairobi_l);
};
var _elm_community$elm_time$Time_TimeZones$africa_monrovia = function (_p1098) {
	var _p1099 = _p1098;
	return _elm_lang$lazy$Lazy$force(_elm_community$elm_time$Time_TimeZoneData$africa_monrovia_l);
};
var _elm_community$elm_time$Time_TimeZones$africa_mogadishu = function (_p1100) {
	var _p1101 = _p1100;
	return _elm_lang$lazy$Lazy$force(
		A2(_elm_community$elm_time$Time_TimeZoneData$link, 'Africa/Nairobi', _elm_community$elm_time$Time_TimeZoneData$africa_nairobi_l));
};
var _elm_community$elm_time$Time_TimeZones$africa_mbabane = function (_p1102) {
	var _p1103 = _p1102;
	return _elm_lang$lazy$Lazy$force(
		A2(_elm_community$elm_time$Time_TimeZoneData$link, 'Africa/Johannesburg', _elm_community$elm_time$Time_TimeZoneData$africa_johannesburg_l));
};
var _elm_community$elm_time$Time_TimeZones$africa_maseru = function (_p1104) {
	var _p1105 = _p1104;
	return _elm_lang$lazy$Lazy$force(
		A2(_elm_community$elm_time$Time_TimeZoneData$link, 'Africa/Johannesburg', _elm_community$elm_time$Time_TimeZoneData$africa_johannesburg_l));
};
var _elm_community$elm_time$Time_TimeZones$africa_maputo = function (_p1106) {
	var _p1107 = _p1106;
	return _elm_lang$lazy$Lazy$force(_elm_community$elm_time$Time_TimeZoneData$africa_maputo_l);
};
var _elm_community$elm_time$Time_TimeZones$africa_malabo = function (_p1108) {
	var _p1109 = _p1108;
	return _elm_lang$lazy$Lazy$force(
		A2(_elm_community$elm_time$Time_TimeZoneData$link, 'Africa/Lagos', _elm_community$elm_time$Time_TimeZoneData$africa_lagos_l));
};
var _elm_community$elm_time$Time_TimeZones$africa_lusaka = function (_p1110) {
	var _p1111 = _p1110;
	return _elm_lang$lazy$Lazy$force(
		A2(_elm_community$elm_time$Time_TimeZoneData$link, 'Africa/Maputo', _elm_community$elm_time$Time_TimeZoneData$africa_maputo_l));
};
var _elm_community$elm_time$Time_TimeZones$africa_lubumbashi = function (_p1112) {
	var _p1113 = _p1112;
	return _elm_lang$lazy$Lazy$force(
		A2(_elm_community$elm_time$Time_TimeZoneData$link, 'Africa/Maputo', _elm_community$elm_time$Time_TimeZoneData$africa_maputo_l));
};
var _elm_community$elm_time$Time_TimeZones$africa_luanda = function (_p1114) {
	var _p1115 = _p1114;
	return _elm_lang$lazy$Lazy$force(
		A2(_elm_community$elm_time$Time_TimeZoneData$link, 'Africa/Lagos', _elm_community$elm_time$Time_TimeZoneData$africa_lagos_l));
};
var _elm_community$elm_time$Time_TimeZones$africa_lome = function (_p1116) {
	var _p1117 = _p1116;
	return _elm_lang$lazy$Lazy$force(
		A2(_elm_community$elm_time$Time_TimeZoneData$link, 'Africa/Abidjan', _elm_community$elm_time$Time_TimeZoneData$africa_abidjan_l));
};
var _elm_community$elm_time$Time_TimeZones$africa_libreville = function (_p1118) {
	var _p1119 = _p1118;
	return _elm_lang$lazy$Lazy$force(
		A2(_elm_community$elm_time$Time_TimeZoneData$link, 'Africa/Lagos', _elm_community$elm_time$Time_TimeZoneData$africa_lagos_l));
};
var _elm_community$elm_time$Time_TimeZones$africa_lagos = function (_p1120) {
	var _p1121 = _p1120;
	return _elm_lang$lazy$Lazy$force(_elm_community$elm_time$Time_TimeZoneData$africa_lagos_l);
};
var _elm_community$elm_time$Time_TimeZones$africa_kinshasa = function (_p1122) {
	var _p1123 = _p1122;
	return _elm_lang$lazy$Lazy$force(
		A2(_elm_community$elm_time$Time_TimeZoneData$link, 'Africa/Lagos', _elm_community$elm_time$Time_TimeZoneData$africa_lagos_l));
};
var _elm_community$elm_time$Time_TimeZones$africa_kigali = function (_p1124) {
	var _p1125 = _p1124;
	return _elm_lang$lazy$Lazy$force(
		A2(_elm_community$elm_time$Time_TimeZoneData$link, 'Africa/Maputo', _elm_community$elm_time$Time_TimeZoneData$africa_maputo_l));
};
var _elm_community$elm_time$Time_TimeZones$africa_khartoum = function (_p1126) {
	var _p1127 = _p1126;
	return _elm_lang$lazy$Lazy$force(_elm_community$elm_time$Time_TimeZoneData$africa_khartoum_l);
};
var _elm_community$elm_time$Time_TimeZones$africa_kampala = function (_p1128) {
	var _p1129 = _p1128;
	return _elm_lang$lazy$Lazy$force(
		A2(_elm_community$elm_time$Time_TimeZoneData$link, 'Africa/Nairobi', _elm_community$elm_time$Time_TimeZoneData$africa_nairobi_l));
};
var _elm_community$elm_time$Time_TimeZones$africa_juba = function (_p1130) {
	var _p1131 = _p1130;
	return _elm_lang$lazy$Lazy$force(
		A2(_elm_community$elm_time$Time_TimeZoneData$link, 'Africa/Khartoum', _elm_community$elm_time$Time_TimeZoneData$africa_khartoum_l));
};
var _elm_community$elm_time$Time_TimeZones$africa_johannesburg = function (_p1132) {
	var _p1133 = _p1132;
	return _elm_lang$lazy$Lazy$force(_elm_community$elm_time$Time_TimeZoneData$africa_johannesburg_l);
};
var _elm_community$elm_time$Time_TimeZones$africa_harare = function (_p1134) {
	var _p1135 = _p1134;
	return _elm_lang$lazy$Lazy$force(
		A2(_elm_community$elm_time$Time_TimeZoneData$link, 'Africa/Maputo', _elm_community$elm_time$Time_TimeZoneData$africa_maputo_l));
};
var _elm_community$elm_time$Time_TimeZones$africa_gaborone = function (_p1136) {
	var _p1137 = _p1136;
	return _elm_lang$lazy$Lazy$force(
		A2(_elm_community$elm_time$Time_TimeZoneData$link, 'Africa/Maputo', _elm_community$elm_time$Time_TimeZoneData$africa_maputo_l));
};
var _elm_community$elm_time$Time_TimeZones$africa_freetown = function (_p1138) {
	var _p1139 = _p1138;
	return _elm_lang$lazy$Lazy$force(
		A2(_elm_community$elm_time$Time_TimeZoneData$link, 'Africa/Abidjan', _elm_community$elm_time$Time_TimeZoneData$africa_abidjan_l));
};
var _elm_community$elm_time$Time_TimeZones$africa_el_aaiun = function (_p1140) {
	var _p1141 = _p1140;
	return _elm_lang$lazy$Lazy$force(_elm_community$elm_time$Time_TimeZoneData$africa_el_aaiun_l);
};
var _elm_community$elm_time$Time_TimeZones$africa_douala = function (_p1142) {
	var _p1143 = _p1142;
	return _elm_lang$lazy$Lazy$force(
		A2(_elm_community$elm_time$Time_TimeZoneData$link, 'Africa/Lagos', _elm_community$elm_time$Time_TimeZoneData$africa_lagos_l));
};
var _elm_community$elm_time$Time_TimeZones$africa_djibouti = function (_p1144) {
	var _p1145 = _p1144;
	return _elm_lang$lazy$Lazy$force(
		A2(_elm_community$elm_time$Time_TimeZoneData$link, 'Africa/Nairobi', _elm_community$elm_time$Time_TimeZoneData$africa_nairobi_l));
};
var _elm_community$elm_time$Time_TimeZones$africa_dar_es_salaam = function (_p1146) {
	var _p1147 = _p1146;
	return _elm_lang$lazy$Lazy$force(
		A2(_elm_community$elm_time$Time_TimeZoneData$link, 'Africa/Nairobi', _elm_community$elm_time$Time_TimeZoneData$africa_nairobi_l));
};
var _elm_community$elm_time$Time_TimeZones$africa_dakar = function (_p1148) {
	var _p1149 = _p1148;
	return _elm_lang$lazy$Lazy$force(
		A2(_elm_community$elm_time$Time_TimeZoneData$link, 'Africa/Abidjan', _elm_community$elm_time$Time_TimeZoneData$africa_abidjan_l));
};
var _elm_community$elm_time$Time_TimeZones$africa_conakry = function (_p1150) {
	var _p1151 = _p1150;
	return _elm_lang$lazy$Lazy$force(
		A2(_elm_community$elm_time$Time_TimeZoneData$link, 'Africa/Abidjan', _elm_community$elm_time$Time_TimeZoneData$africa_abidjan_l));
};
var _elm_community$elm_time$Time_TimeZones$africa_ceuta = function (_p1152) {
	var _p1153 = _p1152;
	return _elm_lang$lazy$Lazy$force(_elm_community$elm_time$Time_TimeZoneData$africa_ceuta_l);
};
var _elm_community$elm_time$Time_TimeZones$africa_casablanca = function (_p1154) {
	var _p1155 = _p1154;
	return _elm_lang$lazy$Lazy$force(_elm_community$elm_time$Time_TimeZoneData$africa_casablanca_l);
};
var _elm_community$elm_time$Time_TimeZones$africa_cairo = function (_p1156) {
	var _p1157 = _p1156;
	return _elm_lang$lazy$Lazy$force(_elm_community$elm_time$Time_TimeZoneData$africa_cairo_l);
};
var _elm_community$elm_time$Time_TimeZones$africa_bujumbura = function (_p1158) {
	var _p1159 = _p1158;
	return _elm_lang$lazy$Lazy$force(
		A2(_elm_community$elm_time$Time_TimeZoneData$link, 'Africa/Maputo', _elm_community$elm_time$Time_TimeZoneData$africa_maputo_l));
};
var _elm_community$elm_time$Time_TimeZones$africa_brazzaville = function (_p1160) {
	var _p1161 = _p1160;
	return _elm_lang$lazy$Lazy$force(
		A2(_elm_community$elm_time$Time_TimeZoneData$link, 'Africa/Lagos', _elm_community$elm_time$Time_TimeZoneData$africa_lagos_l));
};
var _elm_community$elm_time$Time_TimeZones$africa_blantyre = function (_p1162) {
	var _p1163 = _p1162;
	return _elm_lang$lazy$Lazy$force(
		A2(_elm_community$elm_time$Time_TimeZoneData$link, 'Africa/Maputo', _elm_community$elm_time$Time_TimeZoneData$africa_maputo_l));
};
var _elm_community$elm_time$Time_TimeZones$africa_bissau = function (_p1164) {
	var _p1165 = _p1164;
	return _elm_lang$lazy$Lazy$force(_elm_community$elm_time$Time_TimeZoneData$africa_bissau_l);
};
var _elm_community$elm_time$Time_TimeZones$africa_banjul = function (_p1166) {
	var _p1167 = _p1166;
	return _elm_lang$lazy$Lazy$force(
		A2(_elm_community$elm_time$Time_TimeZoneData$link, 'Africa/Abidjan', _elm_community$elm_time$Time_TimeZoneData$africa_abidjan_l));
};
var _elm_community$elm_time$Time_TimeZones$africa_bangui = function (_p1168) {
	var _p1169 = _p1168;
	return _elm_lang$lazy$Lazy$force(
		A2(_elm_community$elm_time$Time_TimeZoneData$link, 'Africa/Lagos', _elm_community$elm_time$Time_TimeZoneData$africa_lagos_l));
};
var _elm_community$elm_time$Time_TimeZones$africa_bamako = function (_p1170) {
	var _p1171 = _p1170;
	return _elm_lang$lazy$Lazy$force(
		A2(_elm_community$elm_time$Time_TimeZoneData$link, 'Africa/Abidjan', _elm_community$elm_time$Time_TimeZoneData$africa_abidjan_l));
};
var _elm_community$elm_time$Time_TimeZones$africa_asmera = function (_p1172) {
	var _p1173 = _p1172;
	return _elm_lang$lazy$Lazy$force(
		A2(_elm_community$elm_time$Time_TimeZoneData$link, 'Africa/Nairobi', _elm_community$elm_time$Time_TimeZoneData$africa_nairobi_l));
};
var _elm_community$elm_time$Time_TimeZones$africa_asmara = function (_p1174) {
	var _p1175 = _p1174;
	return _elm_lang$lazy$Lazy$force(
		A2(_elm_community$elm_time$Time_TimeZoneData$link, 'Africa/Nairobi', _elm_community$elm_time$Time_TimeZoneData$africa_nairobi_l));
};
var _elm_community$elm_time$Time_TimeZones$africa_algiers = function (_p1176) {
	var _p1177 = _p1176;
	return _elm_lang$lazy$Lazy$force(_elm_community$elm_time$Time_TimeZoneData$africa_algiers_l);
};
var _elm_community$elm_time$Time_TimeZones$africa_addis_ababa = function (_p1178) {
	var _p1179 = _p1178;
	return _elm_lang$lazy$Lazy$force(
		A2(_elm_community$elm_time$Time_TimeZoneData$link, 'Africa/Nairobi', _elm_community$elm_time$Time_TimeZoneData$africa_nairobi_l));
};
var _elm_community$elm_time$Time_TimeZones$africa_accra = function (_p1180) {
	var _p1181 = _p1180;
	return _elm_lang$lazy$Lazy$force(_elm_community$elm_time$Time_TimeZoneData$africa_accra_l);
};
var _elm_community$elm_time$Time_TimeZones$africa_abidjan = function (_p1182) {
	var _p1183 = _p1182;
	return _elm_lang$lazy$Lazy$force(_elm_community$elm_time$Time_TimeZoneData$africa_abidjan_l);
};
var _elm_community$elm_time$Time_TimeZones$all = _elm_lang$core$Dict$fromList(
	_elm_lang$core$List$concat(
		{
			ctor: '::',
			_0: {
				ctor: '::',
				_0: {ctor: '_Tuple2', _0: 'Africa/Abidjan', _1: _elm_community$elm_time$Time_TimeZones$africa_abidjan},
				_1: {
					ctor: '::',
					_0: {ctor: '_Tuple2', _0: 'Africa/Accra', _1: _elm_community$elm_time$Time_TimeZones$africa_accra},
					_1: {
						ctor: '::',
						_0: {ctor: '_Tuple2', _0: 'Africa/Nairobi', _1: _elm_community$elm_time$Time_TimeZones$africa_addis_ababa},
						_1: {
							ctor: '::',
							_0: {ctor: '_Tuple2', _0: 'Africa/Algiers', _1: _elm_community$elm_time$Time_TimeZones$africa_algiers},
							_1: {
								ctor: '::',
								_0: {ctor: '_Tuple2', _0: 'Africa/Nairobi', _1: _elm_community$elm_time$Time_TimeZones$africa_asmara},
								_1: {
									ctor: '::',
									_0: {ctor: '_Tuple2', _0: 'Africa/Nairobi', _1: _elm_community$elm_time$Time_TimeZones$africa_asmera},
									_1: {
										ctor: '::',
										_0: {ctor: '_Tuple2', _0: 'Africa/Abidjan', _1: _elm_community$elm_time$Time_TimeZones$africa_bamako},
										_1: {
											ctor: '::',
											_0: {ctor: '_Tuple2', _0: 'Africa/Lagos', _1: _elm_community$elm_time$Time_TimeZones$africa_bangui},
											_1: {
												ctor: '::',
												_0: {ctor: '_Tuple2', _0: 'Africa/Abidjan', _1: _elm_community$elm_time$Time_TimeZones$africa_banjul},
												_1: {
													ctor: '::',
													_0: {ctor: '_Tuple2', _0: 'Africa/Bissau', _1: _elm_community$elm_time$Time_TimeZones$africa_bissau},
													_1: {
														ctor: '::',
														_0: {ctor: '_Tuple2', _0: 'Africa/Maputo', _1: _elm_community$elm_time$Time_TimeZones$africa_blantyre},
														_1: {
															ctor: '::',
															_0: {ctor: '_Tuple2', _0: 'Africa/Lagos', _1: _elm_community$elm_time$Time_TimeZones$africa_brazzaville},
															_1: {
																ctor: '::',
																_0: {ctor: '_Tuple2', _0: 'Africa/Maputo', _1: _elm_community$elm_time$Time_TimeZones$africa_bujumbura},
																_1: {
																	ctor: '::',
																	_0: {ctor: '_Tuple2', _0: 'Africa/Cairo', _1: _elm_community$elm_time$Time_TimeZones$africa_cairo},
																	_1: {
																		ctor: '::',
																		_0: {ctor: '_Tuple2', _0: 'Africa/Casablanca', _1: _elm_community$elm_time$Time_TimeZones$africa_casablanca},
																		_1: {
																			ctor: '::',
																			_0: {ctor: '_Tuple2', _0: 'Africa/Ceuta', _1: _elm_community$elm_time$Time_TimeZones$africa_ceuta},
																			_1: {
																				ctor: '::',
																				_0: {ctor: '_Tuple2', _0: 'Africa/Abidjan', _1: _elm_community$elm_time$Time_TimeZones$africa_conakry},
																				_1: {
																					ctor: '::',
																					_0: {ctor: '_Tuple2', _0: 'Africa/Abidjan', _1: _elm_community$elm_time$Time_TimeZones$africa_dakar},
																					_1: {
																						ctor: '::',
																						_0: {ctor: '_Tuple2', _0: 'Africa/Nairobi', _1: _elm_community$elm_time$Time_TimeZones$africa_dar_es_salaam},
																						_1: {
																							ctor: '::',
																							_0: {ctor: '_Tuple2', _0: 'Africa/Nairobi', _1: _elm_community$elm_time$Time_TimeZones$africa_djibouti},
																							_1: {
																								ctor: '::',
																								_0: {ctor: '_Tuple2', _0: 'Africa/Lagos', _1: _elm_community$elm_time$Time_TimeZones$africa_douala},
																								_1: {
																									ctor: '::',
																									_0: {ctor: '_Tuple2', _0: 'Africa/El_Aaiun', _1: _elm_community$elm_time$Time_TimeZones$africa_el_aaiun},
																									_1: {
																										ctor: '::',
																										_0: {ctor: '_Tuple2', _0: 'Africa/Abidjan', _1: _elm_community$elm_time$Time_TimeZones$africa_freetown},
																										_1: {
																											ctor: '::',
																											_0: {ctor: '_Tuple2', _0: 'Africa/Maputo', _1: _elm_community$elm_time$Time_TimeZones$africa_gaborone},
																											_1: {
																												ctor: '::',
																												_0: {ctor: '_Tuple2', _0: 'Africa/Maputo', _1: _elm_community$elm_time$Time_TimeZones$africa_harare},
																												_1: {
																													ctor: '::',
																													_0: {ctor: '_Tuple2', _0: 'Africa/Johannesburg', _1: _elm_community$elm_time$Time_TimeZones$africa_johannesburg},
																													_1: {
																														ctor: '::',
																														_0: {ctor: '_Tuple2', _0: 'Africa/Khartoum', _1: _elm_community$elm_time$Time_TimeZones$africa_juba},
																														_1: {
																															ctor: '::',
																															_0: {ctor: '_Tuple2', _0: 'Africa/Nairobi', _1: _elm_community$elm_time$Time_TimeZones$africa_kampala},
																															_1: {
																																ctor: '::',
																																_0: {ctor: '_Tuple2', _0: 'Africa/Khartoum', _1: _elm_community$elm_time$Time_TimeZones$africa_khartoum},
																																_1: {
																																	ctor: '::',
																																	_0: {ctor: '_Tuple2', _0: 'Africa/Maputo', _1: _elm_community$elm_time$Time_TimeZones$africa_kigali},
																																	_1: {ctor: '[]'}
																																}
																															}
																														}
																													}
																												}
																											}
																										}
																									}
																								}
																							}
																						}
																					}
																				}
																			}
																		}
																	}
																}
															}
														}
													}
												}
											}
										}
									}
								}
							}
						}
					}
				}
			},
			_1: {
				ctor: '::',
				_0: {
					ctor: '::',
					_0: {ctor: '_Tuple2', _0: 'Africa/Lagos', _1: _elm_community$elm_time$Time_TimeZones$africa_kinshasa},
					_1: {
						ctor: '::',
						_0: {ctor: '_Tuple2', _0: 'Africa/Lagos', _1: _elm_community$elm_time$Time_TimeZones$africa_lagos},
						_1: {
							ctor: '::',
							_0: {ctor: '_Tuple2', _0: 'Africa/Lagos', _1: _elm_community$elm_time$Time_TimeZones$africa_libreville},
							_1: {
								ctor: '::',
								_0: {ctor: '_Tuple2', _0: 'Africa/Abidjan', _1: _elm_community$elm_time$Time_TimeZones$africa_lome},
								_1: {
									ctor: '::',
									_0: {ctor: '_Tuple2', _0: 'Africa/Lagos', _1: _elm_community$elm_time$Time_TimeZones$africa_luanda},
									_1: {
										ctor: '::',
										_0: {ctor: '_Tuple2', _0: 'Africa/Maputo', _1: _elm_community$elm_time$Time_TimeZones$africa_lubumbashi},
										_1: {
											ctor: '::',
											_0: {ctor: '_Tuple2', _0: 'Africa/Maputo', _1: _elm_community$elm_time$Time_TimeZones$africa_lusaka},
											_1: {
												ctor: '::',
												_0: {ctor: '_Tuple2', _0: 'Africa/Lagos', _1: _elm_community$elm_time$Time_TimeZones$africa_malabo},
												_1: {
													ctor: '::',
													_0: {ctor: '_Tuple2', _0: 'Africa/Maputo', _1: _elm_community$elm_time$Time_TimeZones$africa_maputo},
													_1: {
														ctor: '::',
														_0: {ctor: '_Tuple2', _0: 'Africa/Johannesburg', _1: _elm_community$elm_time$Time_TimeZones$africa_maseru},
														_1: {
															ctor: '::',
															_0: {ctor: '_Tuple2', _0: 'Africa/Johannesburg', _1: _elm_community$elm_time$Time_TimeZones$africa_mbabane},
															_1: {
																ctor: '::',
																_0: {ctor: '_Tuple2', _0: 'Africa/Nairobi', _1: _elm_community$elm_time$Time_TimeZones$africa_mogadishu},
																_1: {
																	ctor: '::',
																	_0: {ctor: '_Tuple2', _0: 'Africa/Monrovia', _1: _elm_community$elm_time$Time_TimeZones$africa_monrovia},
																	_1: {
																		ctor: '::',
																		_0: {ctor: '_Tuple2', _0: 'Africa/Nairobi', _1: _elm_community$elm_time$Time_TimeZones$africa_nairobi},
																		_1: {
																			ctor: '::',
																			_0: {ctor: '_Tuple2', _0: 'Africa/Ndjamena', _1: _elm_community$elm_time$Time_TimeZones$africa_ndjamena},
																			_1: {
																				ctor: '::',
																				_0: {ctor: '_Tuple2', _0: 'Africa/Lagos', _1: _elm_community$elm_time$Time_TimeZones$africa_niamey},
																				_1: {
																					ctor: '::',
																					_0: {ctor: '_Tuple2', _0: 'Africa/Abidjan', _1: _elm_community$elm_time$Time_TimeZones$africa_nouakchott},
																					_1: {
																						ctor: '::',
																						_0: {ctor: '_Tuple2', _0: 'Africa/Abidjan', _1: _elm_community$elm_time$Time_TimeZones$africa_ouagadougou},
																						_1: {
																							ctor: '::',
																							_0: {ctor: '_Tuple2', _0: 'Africa/Lagos', _1: _elm_community$elm_time$Time_TimeZones$africa_porto_novo},
																							_1: {
																								ctor: '::',
																								_0: {ctor: '_Tuple2', _0: 'Africa/Abidjan', _1: _elm_community$elm_time$Time_TimeZones$africa_sao_tome},
																								_1: {
																									ctor: '::',
																									_0: {ctor: '_Tuple2', _0: 'Africa/Abidjan', _1: _elm_community$elm_time$Time_TimeZones$africa_timbuktu},
																									_1: {
																										ctor: '::',
																										_0: {ctor: '_Tuple2', _0: 'Africa/Tripoli', _1: _elm_community$elm_time$Time_TimeZones$africa_tripoli},
																										_1: {
																											ctor: '::',
																											_0: {ctor: '_Tuple2', _0: 'Africa/Tunis', _1: _elm_community$elm_time$Time_TimeZones$africa_tunis},
																											_1: {
																												ctor: '::',
																												_0: {ctor: '_Tuple2', _0: 'Africa/Windhoek', _1: _elm_community$elm_time$Time_TimeZones$africa_windhoek},
																												_1: {
																													ctor: '::',
																													_0: {ctor: '_Tuple2', _0: 'America/Adak', _1: _elm_community$elm_time$Time_TimeZones$america_adak},
																													_1: {
																														ctor: '::',
																														_0: {ctor: '_Tuple2', _0: 'America/Anchorage', _1: _elm_community$elm_time$Time_TimeZones$america_anchorage},
																														_1: {
																															ctor: '::',
																															_0: {ctor: '_Tuple2', _0: 'America/Port_of_Spain', _1: _elm_community$elm_time$Time_TimeZones$america_anguilla},
																															_1: {
																																ctor: '::',
																																_0: {ctor: '_Tuple2', _0: 'America/Port_of_Spain', _1: _elm_community$elm_time$Time_TimeZones$america_antigua},
																																_1: {
																																	ctor: '::',
																																	_0: {ctor: '_Tuple2', _0: 'America/Araguaina', _1: _elm_community$elm_time$Time_TimeZones$america_araguaina},
																																	_1: {
																																		ctor: '::',
																																		_0: {ctor: '_Tuple2', _0: 'America/Argentina/Buenos_Aires', _1: _elm_community$elm_time$Time_TimeZones$america_argentina_buenos_aires},
																																		_1: {ctor: '[]'}
																																	}
																																}
																															}
																														}
																													}
																												}
																											}
																										}
																									}
																								}
																							}
																						}
																					}
																				}
																			}
																		}
																	}
																}
															}
														}
													}
												}
											}
										}
									}
								}
							}
						}
					}
				},
				_1: {
					ctor: '::',
					_0: {
						ctor: '::',
						_0: {ctor: '_Tuple2', _0: 'America/Argentina/Catamarca', _1: _elm_community$elm_time$Time_TimeZones$america_argentina_catamarca},
						_1: {
							ctor: '::',
							_0: {ctor: '_Tuple2', _0: 'America/Argentina/Catamarca', _1: _elm_community$elm_time$Time_TimeZones$america_argentina_comodrivadavia},
							_1: {
								ctor: '::',
								_0: {ctor: '_Tuple2', _0: 'America/Argentina/Cordoba', _1: _elm_community$elm_time$Time_TimeZones$america_argentina_cordoba},
								_1: {
									ctor: '::',
									_0: {ctor: '_Tuple2', _0: 'America/Argentina/Jujuy', _1: _elm_community$elm_time$Time_TimeZones$america_argentina_jujuy},
									_1: {
										ctor: '::',
										_0: {ctor: '_Tuple2', _0: 'America/Argentina/La_Rioja', _1: _elm_community$elm_time$Time_TimeZones$america_argentina_la_rioja},
										_1: {
											ctor: '::',
											_0: {ctor: '_Tuple2', _0: 'America/Argentina/Mendoza', _1: _elm_community$elm_time$Time_TimeZones$america_argentina_mendoza},
											_1: {
												ctor: '::',
												_0: {ctor: '_Tuple2', _0: 'America/Argentina/Rio_Gallegos', _1: _elm_community$elm_time$Time_TimeZones$america_argentina_rio_gallegos},
												_1: {
													ctor: '::',
													_0: {ctor: '_Tuple2', _0: 'America/Argentina/Salta', _1: _elm_community$elm_time$Time_TimeZones$america_argentina_salta},
													_1: {
														ctor: '::',
														_0: {ctor: '_Tuple2', _0: 'America/Argentina/San_Juan', _1: _elm_community$elm_time$Time_TimeZones$america_argentina_san_juan},
														_1: {
															ctor: '::',
															_0: {ctor: '_Tuple2', _0: 'America/Argentina/San_Luis', _1: _elm_community$elm_time$Time_TimeZones$america_argentina_san_luis},
															_1: {
																ctor: '::',
																_0: {ctor: '_Tuple2', _0: 'America/Argentina/Tucuman', _1: _elm_community$elm_time$Time_TimeZones$america_argentina_tucuman},
																_1: {
																	ctor: '::',
																	_0: {ctor: '_Tuple2', _0: 'America/Argentina/Ushuaia', _1: _elm_community$elm_time$Time_TimeZones$america_argentina_ushuaia},
																	_1: {
																		ctor: '::',
																		_0: {ctor: '_Tuple2', _0: 'America/Curacao', _1: _elm_community$elm_time$Time_TimeZones$america_aruba},
																		_1: {
																			ctor: '::',
																			_0: {ctor: '_Tuple2', _0: 'America/Asuncion', _1: _elm_community$elm_time$Time_TimeZones$america_asuncion},
																			_1: {
																				ctor: '::',
																				_0: {ctor: '_Tuple2', _0: 'America/Atikokan', _1: _elm_community$elm_time$Time_TimeZones$america_atikokan},
																				_1: {
																					ctor: '::',
																					_0: {ctor: '_Tuple2', _0: 'America/Adak', _1: _elm_community$elm_time$Time_TimeZones$america_atka},
																					_1: {
																						ctor: '::',
																						_0: {ctor: '_Tuple2', _0: 'America/Bahia', _1: _elm_community$elm_time$Time_TimeZones$america_bahia},
																						_1: {
																							ctor: '::',
																							_0: {ctor: '_Tuple2', _0: 'America/Bahia_Banderas', _1: _elm_community$elm_time$Time_TimeZones$america_bahia_banderas},
																							_1: {
																								ctor: '::',
																								_0: {ctor: '_Tuple2', _0: 'America/Barbados', _1: _elm_community$elm_time$Time_TimeZones$america_barbados},
																								_1: {
																									ctor: '::',
																									_0: {ctor: '_Tuple2', _0: 'America/Belem', _1: _elm_community$elm_time$Time_TimeZones$america_belem},
																									_1: {
																										ctor: '::',
																										_0: {ctor: '_Tuple2', _0: 'America/Belize', _1: _elm_community$elm_time$Time_TimeZones$america_belize},
																										_1: {
																											ctor: '::',
																											_0: {ctor: '_Tuple2', _0: 'America/Blanc-Sablon', _1: _elm_community$elm_time$Time_TimeZones$america_blanc_sablon},
																											_1: {
																												ctor: '::',
																												_0: {ctor: '_Tuple2', _0: 'America/Boa_Vista', _1: _elm_community$elm_time$Time_TimeZones$america_boa_vista},
																												_1: {
																													ctor: '::',
																													_0: {ctor: '_Tuple2', _0: 'America/Bogota', _1: _elm_community$elm_time$Time_TimeZones$america_bogota},
																													_1: {
																														ctor: '::',
																														_0: {ctor: '_Tuple2', _0: 'America/Boise', _1: _elm_community$elm_time$Time_TimeZones$america_boise},
																														_1: {
																															ctor: '::',
																															_0: {ctor: '_Tuple2', _0: 'America/Argentina/Buenos_Aires', _1: _elm_community$elm_time$Time_TimeZones$america_buenos_aires},
																															_1: {
																																ctor: '::',
																																_0: {ctor: '_Tuple2', _0: 'America/Cambridge_Bay', _1: _elm_community$elm_time$Time_TimeZones$america_cambridge_bay},
																																_1: {
																																	ctor: '::',
																																	_0: {ctor: '_Tuple2', _0: 'America/Campo_Grande', _1: _elm_community$elm_time$Time_TimeZones$america_campo_grande},
																																	_1: {
																																		ctor: '::',
																																		_0: {ctor: '_Tuple2', _0: 'America/Cancun', _1: _elm_community$elm_time$Time_TimeZones$america_cancun},
																																		_1: {
																																			ctor: '::',
																																			_0: {ctor: '_Tuple2', _0: 'America/Caracas', _1: _elm_community$elm_time$Time_TimeZones$america_caracas},
																																			_1: {ctor: '[]'}
																																		}
																																	}
																																}
																															}
																														}
																													}
																												}
																											}
																										}
																									}
																								}
																							}
																						}
																					}
																				}
																			}
																		}
																	}
																}
															}
														}
													}
												}
											}
										}
									}
								}
							}
						}
					},
					_1: {
						ctor: '::',
						_0: {
							ctor: '::',
							_0: {ctor: '_Tuple2', _0: 'America/Argentina/Catamarca', _1: _elm_community$elm_time$Time_TimeZones$america_catamarca},
							_1: {
								ctor: '::',
								_0: {ctor: '_Tuple2', _0: 'America/Cayenne', _1: _elm_community$elm_time$Time_TimeZones$america_cayenne},
								_1: {
									ctor: '::',
									_0: {ctor: '_Tuple2', _0: 'America/Panama', _1: _elm_community$elm_time$Time_TimeZones$america_cayman},
									_1: {
										ctor: '::',
										_0: {ctor: '_Tuple2', _0: 'America/Chicago', _1: _elm_community$elm_time$Time_TimeZones$america_chicago},
										_1: {
											ctor: '::',
											_0: {ctor: '_Tuple2', _0: 'America/Chihuahua', _1: _elm_community$elm_time$Time_TimeZones$america_chihuahua},
											_1: {
												ctor: '::',
												_0: {ctor: '_Tuple2', _0: 'America/Atikokan', _1: _elm_community$elm_time$Time_TimeZones$america_coral_harbour},
												_1: {
													ctor: '::',
													_0: {ctor: '_Tuple2', _0: 'America/Argentina/Cordoba', _1: _elm_community$elm_time$Time_TimeZones$america_cordoba},
													_1: {
														ctor: '::',
														_0: {ctor: '_Tuple2', _0: 'America/Costa_Rica', _1: _elm_community$elm_time$Time_TimeZones$america_costa_rica},
														_1: {
															ctor: '::',
															_0: {ctor: '_Tuple2', _0: 'America/Creston', _1: _elm_community$elm_time$Time_TimeZones$america_creston},
															_1: {
																ctor: '::',
																_0: {ctor: '_Tuple2', _0: 'America/Cuiaba', _1: _elm_community$elm_time$Time_TimeZones$america_cuiaba},
																_1: {
																	ctor: '::',
																	_0: {ctor: '_Tuple2', _0: 'America/Curacao', _1: _elm_community$elm_time$Time_TimeZones$america_curacao},
																	_1: {
																		ctor: '::',
																		_0: {ctor: '_Tuple2', _0: 'America/Danmarkshavn', _1: _elm_community$elm_time$Time_TimeZones$america_danmarkshavn},
																		_1: {
																			ctor: '::',
																			_0: {ctor: '_Tuple2', _0: 'America/Dawson', _1: _elm_community$elm_time$Time_TimeZones$america_dawson},
																			_1: {
																				ctor: '::',
																				_0: {ctor: '_Tuple2', _0: 'America/Dawson_Creek', _1: _elm_community$elm_time$Time_TimeZones$america_dawson_creek},
																				_1: {
																					ctor: '::',
																					_0: {ctor: '_Tuple2', _0: 'America/Denver', _1: _elm_community$elm_time$Time_TimeZones$america_denver},
																					_1: {
																						ctor: '::',
																						_0: {ctor: '_Tuple2', _0: 'America/Detroit', _1: _elm_community$elm_time$Time_TimeZones$america_detroit},
																						_1: {
																							ctor: '::',
																							_0: {ctor: '_Tuple2', _0: 'America/Port_of_Spain', _1: _elm_community$elm_time$Time_TimeZones$america_dominica},
																							_1: {
																								ctor: '::',
																								_0: {ctor: '_Tuple2', _0: 'America/Edmonton', _1: _elm_community$elm_time$Time_TimeZones$america_edmonton},
																								_1: {
																									ctor: '::',
																									_0: {ctor: '_Tuple2', _0: 'America/Eirunepe', _1: _elm_community$elm_time$Time_TimeZones$america_eirunepe},
																									_1: {
																										ctor: '::',
																										_0: {ctor: '_Tuple2', _0: 'America/El_Salvador', _1: _elm_community$elm_time$Time_TimeZones$america_el_salvador},
																										_1: {
																											ctor: '::',
																											_0: {ctor: '_Tuple2', _0: 'America/Tijuana', _1: _elm_community$elm_time$Time_TimeZones$america_ensenada},
																											_1: {
																												ctor: '::',
																												_0: {ctor: '_Tuple2', _0: 'America/Fort_Nelson', _1: _elm_community$elm_time$Time_TimeZones$america_fort_nelson},
																												_1: {
																													ctor: '::',
																													_0: {ctor: '_Tuple2', _0: 'America/Fort_Wayne', _1: _elm_community$elm_time$Time_TimeZones$america_fort_wayne},
																													_1: {
																														ctor: '::',
																														_0: {ctor: '_Tuple2', _0: 'America/Fortaleza', _1: _elm_community$elm_time$Time_TimeZones$america_fortaleza},
																														_1: {
																															ctor: '::',
																															_0: {ctor: '_Tuple2', _0: 'America/Glace_Bay', _1: _elm_community$elm_time$Time_TimeZones$america_glace_bay},
																															_1: {
																																ctor: '::',
																																_0: {ctor: '_Tuple2', _0: 'America/Godthab', _1: _elm_community$elm_time$Time_TimeZones$america_godthab},
																																_1: {
																																	ctor: '::',
																																	_0: {ctor: '_Tuple2', _0: 'America/Goose_Bay', _1: _elm_community$elm_time$Time_TimeZones$america_goose_bay},
																																	_1: {
																																		ctor: '::',
																																		_0: {ctor: '_Tuple2', _0: 'America/Grand_Turk', _1: _elm_community$elm_time$Time_TimeZones$america_grand_turk},
																																		_1: {
																																			ctor: '::',
																																			_0: {ctor: '_Tuple2', _0: 'America/Port_of_Spain', _1: _elm_community$elm_time$Time_TimeZones$america_grenada},
																																			_1: {
																																				ctor: '::',
																																				_0: {ctor: '_Tuple2', _0: 'America/Port_of_Spain', _1: _elm_community$elm_time$Time_TimeZones$america_guadeloupe},
																																				_1: {ctor: '[]'}
																																			}
																																		}
																																	}
																																}
																															}
																														}
																													}
																												}
																											}
																										}
																									}
																								}
																							}
																						}
																					}
																				}
																			}
																		}
																	}
																}
															}
														}
													}
												}
											}
										}
									}
								}
							}
						},
						_1: {
							ctor: '::',
							_0: {
								ctor: '::',
								_0: {ctor: '_Tuple2', _0: 'America/Guatemala', _1: _elm_community$elm_time$Time_TimeZones$america_guatemala},
								_1: {
									ctor: '::',
									_0: {ctor: '_Tuple2', _0: 'America/Guayaquil', _1: _elm_community$elm_time$Time_TimeZones$america_guayaquil},
									_1: {
										ctor: '::',
										_0: {ctor: '_Tuple2', _0: 'America/Guyana', _1: _elm_community$elm_time$Time_TimeZones$america_guyana},
										_1: {
											ctor: '::',
											_0: {ctor: '_Tuple2', _0: 'America/Halifax', _1: _elm_community$elm_time$Time_TimeZones$america_halifax},
											_1: {
												ctor: '::',
												_0: {ctor: '_Tuple2', _0: 'America/Havana', _1: _elm_community$elm_time$Time_TimeZones$america_havana},
												_1: {
													ctor: '::',
													_0: {ctor: '_Tuple2', _0: 'America/Hermosillo', _1: _elm_community$elm_time$Time_TimeZones$america_hermosillo},
													_1: {
														ctor: '::',
														_0: {ctor: '_Tuple2', _0: 'America/Fort_Wayne', _1: _elm_community$elm_time$Time_TimeZones$america_indiana_indianapolis},
														_1: {
															ctor: '::',
															_0: {ctor: '_Tuple2', _0: 'America/Indiana/Knox', _1: _elm_community$elm_time$Time_TimeZones$america_indiana_knox},
															_1: {
																ctor: '::',
																_0: {ctor: '_Tuple2', _0: 'America/Indiana/Marengo', _1: _elm_community$elm_time$Time_TimeZones$america_indiana_marengo},
																_1: {
																	ctor: '::',
																	_0: {ctor: '_Tuple2', _0: 'America/Indiana/Petersburg', _1: _elm_community$elm_time$Time_TimeZones$america_indiana_petersburg},
																	_1: {
																		ctor: '::',
																		_0: {ctor: '_Tuple2', _0: 'America/Indiana/Tell_City', _1: _elm_community$elm_time$Time_TimeZones$america_indiana_tell_city},
																		_1: {
																			ctor: '::',
																			_0: {ctor: '_Tuple2', _0: 'America/Indiana/Vevay', _1: _elm_community$elm_time$Time_TimeZones$america_indiana_vevay},
																			_1: {
																				ctor: '::',
																				_0: {ctor: '_Tuple2', _0: 'America/Indiana/Vincennes', _1: _elm_community$elm_time$Time_TimeZones$america_indiana_vincennes},
																				_1: {
																					ctor: '::',
																					_0: {ctor: '_Tuple2', _0: 'America/Indiana/Winamac', _1: _elm_community$elm_time$Time_TimeZones$america_indiana_winamac},
																					_1: {
																						ctor: '::',
																						_0: {ctor: '_Tuple2', _0: 'America/Fort_Wayne', _1: _elm_community$elm_time$Time_TimeZones$america_indianapolis},
																						_1: {
																							ctor: '::',
																							_0: {ctor: '_Tuple2', _0: 'America/Inuvik', _1: _elm_community$elm_time$Time_TimeZones$america_inuvik},
																							_1: {
																								ctor: '::',
																								_0: {ctor: '_Tuple2', _0: 'America/Iqaluit', _1: _elm_community$elm_time$Time_TimeZones$america_iqaluit},
																								_1: {
																									ctor: '::',
																									_0: {ctor: '_Tuple2', _0: 'America/Jamaica', _1: _elm_community$elm_time$Time_TimeZones$america_jamaica},
																									_1: {
																										ctor: '::',
																										_0: {ctor: '_Tuple2', _0: 'America/Argentina/Jujuy', _1: _elm_community$elm_time$Time_TimeZones$america_jujuy},
																										_1: {
																											ctor: '::',
																											_0: {ctor: '_Tuple2', _0: 'America/Juneau', _1: _elm_community$elm_time$Time_TimeZones$america_juneau},
																											_1: {
																												ctor: '::',
																												_0: {ctor: '_Tuple2', _0: 'America/Kentucky/Louisville', _1: _elm_community$elm_time$Time_TimeZones$america_kentucky_louisville},
																												_1: {
																													ctor: '::',
																													_0: {ctor: '_Tuple2', _0: 'America/Kentucky/Monticello', _1: _elm_community$elm_time$Time_TimeZones$america_kentucky_monticello},
																													_1: {
																														ctor: '::',
																														_0: {ctor: '_Tuple2', _0: 'America/Indiana/Knox', _1: _elm_community$elm_time$Time_TimeZones$america_knox_in},
																														_1: {
																															ctor: '::',
																															_0: {ctor: '_Tuple2', _0: 'America/Curacao', _1: _elm_community$elm_time$Time_TimeZones$america_kralendijk},
																															_1: {
																																ctor: '::',
																																_0: {ctor: '_Tuple2', _0: 'America/La_Paz', _1: _elm_community$elm_time$Time_TimeZones$america_la_paz},
																																_1: {
																																	ctor: '::',
																																	_0: {ctor: '_Tuple2', _0: 'America/Lima', _1: _elm_community$elm_time$Time_TimeZones$america_lima},
																																	_1: {
																																		ctor: '::',
																																		_0: {ctor: '_Tuple2', _0: 'America/Los_Angeles', _1: _elm_community$elm_time$Time_TimeZones$america_los_angeles},
																																		_1: {
																																			ctor: '::',
																																			_0: {ctor: '_Tuple2', _0: 'America/Kentucky/Louisville', _1: _elm_community$elm_time$Time_TimeZones$america_louisville},
																																			_1: {
																																				ctor: '::',
																																				_0: {ctor: '_Tuple2', _0: 'America/Curacao', _1: _elm_community$elm_time$Time_TimeZones$america_lower_princes},
																																				_1: {
																																					ctor: '::',
																																					_0: {ctor: '_Tuple2', _0: 'America/Maceio', _1: _elm_community$elm_time$Time_TimeZones$america_maceio},
																																					_1: {ctor: '[]'}
																																				}
																																			}
																																		}
																																	}
																																}
																															}
																														}
																													}
																												}
																											}
																										}
																									}
																								}
																							}
																						}
																					}
																				}
																			}
																		}
																	}
																}
															}
														}
													}
												}
											}
										}
									}
								}
							},
							_1: {
								ctor: '::',
								_0: {
									ctor: '::',
									_0: {ctor: '_Tuple2', _0: 'America/Managua', _1: _elm_community$elm_time$Time_TimeZones$america_managua},
									_1: {
										ctor: '::',
										_0: {ctor: '_Tuple2', _0: 'America/Manaus', _1: _elm_community$elm_time$Time_TimeZones$america_manaus},
										_1: {
											ctor: '::',
											_0: {ctor: '_Tuple2', _0: 'America/Port_of_Spain', _1: _elm_community$elm_time$Time_TimeZones$america_marigot},
											_1: {
												ctor: '::',
												_0: {ctor: '_Tuple2', _0: 'America/Martinique', _1: _elm_community$elm_time$Time_TimeZones$america_martinique},
												_1: {
													ctor: '::',
													_0: {ctor: '_Tuple2', _0: 'America/Matamoros', _1: _elm_community$elm_time$Time_TimeZones$america_matamoros},
													_1: {
														ctor: '::',
														_0: {ctor: '_Tuple2', _0: 'America/Mazatlan', _1: _elm_community$elm_time$Time_TimeZones$america_mazatlan},
														_1: {
															ctor: '::',
															_0: {ctor: '_Tuple2', _0: 'America/Argentina/Mendoza', _1: _elm_community$elm_time$Time_TimeZones$america_mendoza},
															_1: {
																ctor: '::',
																_0: {ctor: '_Tuple2', _0: 'America/Menominee', _1: _elm_community$elm_time$Time_TimeZones$america_menominee},
																_1: {
																	ctor: '::',
																	_0: {ctor: '_Tuple2', _0: 'America/Merida', _1: _elm_community$elm_time$Time_TimeZones$america_merida},
																	_1: {
																		ctor: '::',
																		_0: {ctor: '_Tuple2', _0: 'America/Metlakatla', _1: _elm_community$elm_time$Time_TimeZones$america_metlakatla},
																		_1: {
																			ctor: '::',
																			_0: {ctor: '_Tuple2', _0: 'America/Mexico_City', _1: _elm_community$elm_time$Time_TimeZones$america_mexico_city},
																			_1: {
																				ctor: '::',
																				_0: {ctor: '_Tuple2', _0: 'America/Miquelon', _1: _elm_community$elm_time$Time_TimeZones$america_miquelon},
																				_1: {
																					ctor: '::',
																					_0: {ctor: '_Tuple2', _0: 'America/Moncton', _1: _elm_community$elm_time$Time_TimeZones$america_moncton},
																					_1: {
																						ctor: '::',
																						_0: {ctor: '_Tuple2', _0: 'America/Monterrey', _1: _elm_community$elm_time$Time_TimeZones$america_monterrey},
																						_1: {
																							ctor: '::',
																							_0: {ctor: '_Tuple2', _0: 'America/Montevideo', _1: _elm_community$elm_time$Time_TimeZones$america_montevideo},
																							_1: {
																								ctor: '::',
																								_0: {ctor: '_Tuple2', _0: 'America/Toronto', _1: _elm_community$elm_time$Time_TimeZones$america_montreal},
																								_1: {
																									ctor: '::',
																									_0: {ctor: '_Tuple2', _0: 'America/Port_of_Spain', _1: _elm_community$elm_time$Time_TimeZones$america_montserrat},
																									_1: {
																										ctor: '::',
																										_0: {ctor: '_Tuple2', _0: 'America/Nassau', _1: _elm_community$elm_time$Time_TimeZones$america_nassau},
																										_1: {
																											ctor: '::',
																											_0: {ctor: '_Tuple2', _0: 'America/New_York', _1: _elm_community$elm_time$Time_TimeZones$america_new_york},
																											_1: {
																												ctor: '::',
																												_0: {ctor: '_Tuple2', _0: 'America/Nipigon', _1: _elm_community$elm_time$Time_TimeZones$america_nipigon},
																												_1: {
																													ctor: '::',
																													_0: {ctor: '_Tuple2', _0: 'America/Nome', _1: _elm_community$elm_time$Time_TimeZones$america_nome},
																													_1: {
																														ctor: '::',
																														_0: {ctor: '_Tuple2', _0: 'America/Noronha', _1: _elm_community$elm_time$Time_TimeZones$america_noronha},
																														_1: {
																															ctor: '::',
																															_0: {ctor: '_Tuple2', _0: 'America/North_Dakota/Beulah', _1: _elm_community$elm_time$Time_TimeZones$america_north_dakota_beulah},
																															_1: {
																																ctor: '::',
																																_0: {ctor: '_Tuple2', _0: 'America/North_Dakota/Center', _1: _elm_community$elm_time$Time_TimeZones$america_north_dakota_center},
																																_1: {
																																	ctor: '::',
																																	_0: {ctor: '_Tuple2', _0: 'America/North_Dakota/New_Salem', _1: _elm_community$elm_time$Time_TimeZones$america_north_dakota_new_salem},
																																	_1: {
																																		ctor: '::',
																																		_0: {ctor: '_Tuple2', _0: 'America/Ojinaga', _1: _elm_community$elm_time$Time_TimeZones$america_ojinaga},
																																		_1: {
																																			ctor: '::',
																																			_0: {ctor: '_Tuple2', _0: 'America/Panama', _1: _elm_community$elm_time$Time_TimeZones$america_panama},
																																			_1: {
																																				ctor: '::',
																																				_0: {ctor: '_Tuple2', _0: 'America/Pangnirtung', _1: _elm_community$elm_time$Time_TimeZones$america_pangnirtung},
																																				_1: {
																																					ctor: '::',
																																					_0: {ctor: '_Tuple2', _0: 'America/Paramaribo', _1: _elm_community$elm_time$Time_TimeZones$america_paramaribo},
																																					_1: {
																																						ctor: '::',
																																						_0: {ctor: '_Tuple2', _0: 'America/Phoenix', _1: _elm_community$elm_time$Time_TimeZones$america_phoenix},
																																						_1: {ctor: '[]'}
																																					}
																																				}
																																			}
																																		}
																																	}
																																}
																															}
																														}
																													}
																												}
																											}
																										}
																									}
																								}
																							}
																						}
																					}
																				}
																			}
																		}
																	}
																}
															}
														}
													}
												}
											}
										}
									}
								},
								_1: {
									ctor: '::',
									_0: {
										ctor: '::',
										_0: {ctor: '_Tuple2', _0: 'America/Port-au-Prince', _1: _elm_community$elm_time$Time_TimeZones$america_port_au_prince},
										_1: {
											ctor: '::',
											_0: {ctor: '_Tuple2', _0: 'America/Port_of_Spain', _1: _elm_community$elm_time$Time_TimeZones$america_port_of_spain},
											_1: {
												ctor: '::',
												_0: {ctor: '_Tuple2', _0: 'America/Rio_Branco', _1: _elm_community$elm_time$Time_TimeZones$america_porto_acre},
												_1: {
													ctor: '::',
													_0: {ctor: '_Tuple2', _0: 'America/Porto_Velho', _1: _elm_community$elm_time$Time_TimeZones$america_porto_velho},
													_1: {
														ctor: '::',
														_0: {ctor: '_Tuple2', _0: 'America/Puerto_Rico', _1: _elm_community$elm_time$Time_TimeZones$america_puerto_rico},
														_1: {
															ctor: '::',
															_0: {ctor: '_Tuple2', _0: 'America/Rainy_River', _1: _elm_community$elm_time$Time_TimeZones$america_rainy_river},
															_1: {
																ctor: '::',
																_0: {ctor: '_Tuple2', _0: 'America/Rankin_Inlet', _1: _elm_community$elm_time$Time_TimeZones$america_rankin_inlet},
																_1: {
																	ctor: '::',
																	_0: {ctor: '_Tuple2', _0: 'America/Recife', _1: _elm_community$elm_time$Time_TimeZones$america_recife},
																	_1: {
																		ctor: '::',
																		_0: {ctor: '_Tuple2', _0: 'America/Regina', _1: _elm_community$elm_time$Time_TimeZones$america_regina},
																		_1: {
																			ctor: '::',
																			_0: {ctor: '_Tuple2', _0: 'America/Resolute', _1: _elm_community$elm_time$Time_TimeZones$america_resolute},
																			_1: {
																				ctor: '::',
																				_0: {ctor: '_Tuple2', _0: 'America/Rio_Branco', _1: _elm_community$elm_time$Time_TimeZones$america_rio_branco},
																				_1: {
																					ctor: '::',
																					_0: {ctor: '_Tuple2', _0: 'America/Argentina/Cordoba', _1: _elm_community$elm_time$Time_TimeZones$america_rosario},
																					_1: {
																						ctor: '::',
																						_0: {ctor: '_Tuple2', _0: 'America/Tijuana', _1: _elm_community$elm_time$Time_TimeZones$america_santa_isabel},
																						_1: {
																							ctor: '::',
																							_0: {ctor: '_Tuple2', _0: 'America/Santarem', _1: _elm_community$elm_time$Time_TimeZones$america_santarem},
																							_1: {
																								ctor: '::',
																								_0: {ctor: '_Tuple2', _0: 'America/Santiago', _1: _elm_community$elm_time$Time_TimeZones$america_santiago},
																								_1: {
																									ctor: '::',
																									_0: {ctor: '_Tuple2', _0: 'America/Santo_Domingo', _1: _elm_community$elm_time$Time_TimeZones$america_santo_domingo},
																									_1: {
																										ctor: '::',
																										_0: {ctor: '_Tuple2', _0: 'America/Sao_Paulo', _1: _elm_community$elm_time$Time_TimeZones$america_sao_paulo},
																										_1: {
																											ctor: '::',
																											_0: {ctor: '_Tuple2', _0: 'America/Scoresbysund', _1: _elm_community$elm_time$Time_TimeZones$america_scoresbysund},
																											_1: {
																												ctor: '::',
																												_0: {ctor: '_Tuple2', _0: 'America/Denver', _1: _elm_community$elm_time$Time_TimeZones$america_shiprock},
																												_1: {
																													ctor: '::',
																													_0: {ctor: '_Tuple2', _0: 'America/Sitka', _1: _elm_community$elm_time$Time_TimeZones$america_sitka},
																													_1: {
																														ctor: '::',
																														_0: {ctor: '_Tuple2', _0: 'America/Port_of_Spain', _1: _elm_community$elm_time$Time_TimeZones$america_st_barthelemy},
																														_1: {
																															ctor: '::',
																															_0: {ctor: '_Tuple2', _0: 'America/St_Johns', _1: _elm_community$elm_time$Time_TimeZones$america_st_johns},
																															_1: {
																																ctor: '::',
																																_0: {ctor: '_Tuple2', _0: 'America/Port_of_Spain', _1: _elm_community$elm_time$Time_TimeZones$america_st_kitts},
																																_1: {
																																	ctor: '::',
																																	_0: {ctor: '_Tuple2', _0: 'America/Port_of_Spain', _1: _elm_community$elm_time$Time_TimeZones$america_st_lucia},
																																	_1: {
																																		ctor: '::',
																																		_0: {ctor: '_Tuple2', _0: 'America/Port_of_Spain', _1: _elm_community$elm_time$Time_TimeZones$america_st_thomas},
																																		_1: {
																																			ctor: '::',
																																			_0: {ctor: '_Tuple2', _0: 'America/Port_of_Spain', _1: _elm_community$elm_time$Time_TimeZones$america_st_vincent},
																																			_1: {
																																				ctor: '::',
																																				_0: {ctor: '_Tuple2', _0: 'America/Swift_Current', _1: _elm_community$elm_time$Time_TimeZones$america_swift_current},
																																				_1: {
																																					ctor: '::',
																																					_0: {ctor: '_Tuple2', _0: 'America/Tegucigalpa', _1: _elm_community$elm_time$Time_TimeZones$america_tegucigalpa},
																																					_1: {
																																						ctor: '::',
																																						_0: {ctor: '_Tuple2', _0: 'America/Thule', _1: _elm_community$elm_time$Time_TimeZones$america_thule},
																																						_1: {
																																							ctor: '::',
																																							_0: {ctor: '_Tuple2', _0: 'America/Thunder_Bay', _1: _elm_community$elm_time$Time_TimeZones$america_thunder_bay},
																																							_1: {ctor: '[]'}
																																						}
																																					}
																																				}
																																			}
																																		}
																																	}
																																}
																															}
																														}
																													}
																												}
																											}
																										}
																									}
																								}
																							}
																						}
																					}
																				}
																			}
																		}
																	}
																}
															}
														}
													}
												}
											}
										}
									},
									_1: {
										ctor: '::',
										_0: {
											ctor: '::',
											_0: {ctor: '_Tuple2', _0: 'America/Tijuana', _1: _elm_community$elm_time$Time_TimeZones$america_tijuana},
											_1: {
												ctor: '::',
												_0: {ctor: '_Tuple2', _0: 'America/Toronto', _1: _elm_community$elm_time$Time_TimeZones$america_toronto},
												_1: {
													ctor: '::',
													_0: {ctor: '_Tuple2', _0: 'America/Port_of_Spain', _1: _elm_community$elm_time$Time_TimeZones$america_tortola},
													_1: {
														ctor: '::',
														_0: {ctor: '_Tuple2', _0: 'America/Vancouver', _1: _elm_community$elm_time$Time_TimeZones$america_vancouver},
														_1: {
															ctor: '::',
															_0: {ctor: '_Tuple2', _0: 'America/Port_of_Spain', _1: _elm_community$elm_time$Time_TimeZones$america_virgin},
															_1: {
																ctor: '::',
																_0: {ctor: '_Tuple2', _0: 'America/Whitehorse', _1: _elm_community$elm_time$Time_TimeZones$america_whitehorse},
																_1: {
																	ctor: '::',
																	_0: {ctor: '_Tuple2', _0: 'America/Winnipeg', _1: _elm_community$elm_time$Time_TimeZones$america_winnipeg},
																	_1: {
																		ctor: '::',
																		_0: {ctor: '_Tuple2', _0: 'America/Yakutat', _1: _elm_community$elm_time$Time_TimeZones$america_yakutat},
																		_1: {
																			ctor: '::',
																			_0: {ctor: '_Tuple2', _0: 'America/Yellowknife', _1: _elm_community$elm_time$Time_TimeZones$america_yellowknife},
																			_1: {
																				ctor: '::',
																				_0: {ctor: '_Tuple2', _0: 'Antarctica/Casey', _1: _elm_community$elm_time$Time_TimeZones$antarctica_casey},
																				_1: {
																					ctor: '::',
																					_0: {ctor: '_Tuple2', _0: 'Antarctica/Davis', _1: _elm_community$elm_time$Time_TimeZones$antarctica_davis},
																					_1: {
																						ctor: '::',
																						_0: {ctor: '_Tuple2', _0: 'Antarctica/DumontDUrville', _1: _elm_community$elm_time$Time_TimeZones$antarctica_dumontdurville},
																						_1: {
																							ctor: '::',
																							_0: {ctor: '_Tuple2', _0: 'Antarctica/Macquarie', _1: _elm_community$elm_time$Time_TimeZones$antarctica_macquarie},
																							_1: {
																								ctor: '::',
																								_0: {ctor: '_Tuple2', _0: 'Antarctica/Mawson', _1: _elm_community$elm_time$Time_TimeZones$antarctica_mawson},
																								_1: {
																									ctor: '::',
																									_0: {ctor: '_Tuple2', _0: 'Pacific/Auckland', _1: _elm_community$elm_time$Time_TimeZones$antarctica_mcmurdo},
																									_1: {
																										ctor: '::',
																										_0: {ctor: '_Tuple2', _0: 'Antarctica/Palmer', _1: _elm_community$elm_time$Time_TimeZones$antarctica_palmer},
																										_1: {
																											ctor: '::',
																											_0: {ctor: '_Tuple2', _0: 'Antarctica/Rothera', _1: _elm_community$elm_time$Time_TimeZones$antarctica_rothera},
																											_1: {
																												ctor: '::',
																												_0: {ctor: '_Tuple2', _0: 'Pacific/Auckland', _1: _elm_community$elm_time$Time_TimeZones$antarctica_south_pole},
																												_1: {
																													ctor: '::',
																													_0: {ctor: '_Tuple2', _0: 'Antarctica/Syowa', _1: _elm_community$elm_time$Time_TimeZones$antarctica_syowa},
																													_1: {
																														ctor: '::',
																														_0: {ctor: '_Tuple2', _0: 'Antarctica/Troll', _1: _elm_community$elm_time$Time_TimeZones$antarctica_troll},
																														_1: {
																															ctor: '::',
																															_0: {ctor: '_Tuple2', _0: 'Antarctica/Vostok', _1: _elm_community$elm_time$Time_TimeZones$antarctica_vostok},
																															_1: {
																																ctor: '::',
																																_0: {ctor: '_Tuple2', _0: 'Europe/Oslo', _1: _elm_community$elm_time$Time_TimeZones$arctic_longyearbyen},
																																_1: {
																																	ctor: '::',
																																	_0: {ctor: '_Tuple2', _0: 'Asia/Riyadh', _1: _elm_community$elm_time$Time_TimeZones$asia_aden},
																																	_1: {
																																		ctor: '::',
																																		_0: {ctor: '_Tuple2', _0: 'Asia/Almaty', _1: _elm_community$elm_time$Time_TimeZones$asia_almaty},
																																		_1: {
																																			ctor: '::',
																																			_0: {ctor: '_Tuple2', _0: 'Asia/Amman', _1: _elm_community$elm_time$Time_TimeZones$asia_amman},
																																			_1: {
																																				ctor: '::',
																																				_0: {ctor: '_Tuple2', _0: 'Asia/Anadyr', _1: _elm_community$elm_time$Time_TimeZones$asia_anadyr},
																																				_1: {
																																					ctor: '::',
																																					_0: {ctor: '_Tuple2', _0: 'Asia/Aqtau', _1: _elm_community$elm_time$Time_TimeZones$asia_aqtau},
																																					_1: {
																																						ctor: '::',
																																						_0: {ctor: '_Tuple2', _0: 'Asia/Aqtobe', _1: _elm_community$elm_time$Time_TimeZones$asia_aqtobe},
																																						_1: {
																																							ctor: '::',
																																							_0: {ctor: '_Tuple2', _0: 'Asia/Ashgabat', _1: _elm_community$elm_time$Time_TimeZones$asia_ashgabat},
																																							_1: {
																																								ctor: '::',
																																								_0: {ctor: '_Tuple2', _0: 'Asia/Ashgabat', _1: _elm_community$elm_time$Time_TimeZones$asia_ashkhabad},
																																								_1: {ctor: '[]'}
																																							}
																																						}
																																					}
																																				}
																																			}
																																		}
																																	}
																																}
																															}
																														}
																													}
																												}
																											}
																										}
																									}
																								}
																							}
																						}
																					}
																				}
																			}
																		}
																	}
																}
															}
														}
													}
												}
											}
										},
										_1: {
											ctor: '::',
											_0: {
												ctor: '::',
												_0: {ctor: '_Tuple2', _0: 'Asia/Atyrau', _1: _elm_community$elm_time$Time_TimeZones$asia_atyrau},
												_1: {
													ctor: '::',
													_0: {ctor: '_Tuple2', _0: 'Asia/Baghdad', _1: _elm_community$elm_time$Time_TimeZones$asia_baghdad},
													_1: {
														ctor: '::',
														_0: {ctor: '_Tuple2', _0: 'Asia/Qatar', _1: _elm_community$elm_time$Time_TimeZones$asia_bahrain},
														_1: {
															ctor: '::',
															_0: {ctor: '_Tuple2', _0: 'Asia/Baku', _1: _elm_community$elm_time$Time_TimeZones$asia_baku},
															_1: {
																ctor: '::',
																_0: {ctor: '_Tuple2', _0: 'Asia/Bangkok', _1: _elm_community$elm_time$Time_TimeZones$asia_bangkok},
																_1: {
																	ctor: '::',
																	_0: {ctor: '_Tuple2', _0: 'Asia/Barnaul', _1: _elm_community$elm_time$Time_TimeZones$asia_barnaul},
																	_1: {
																		ctor: '::',
																		_0: {ctor: '_Tuple2', _0: 'Asia/Beirut', _1: _elm_community$elm_time$Time_TimeZones$asia_beirut},
																		_1: {
																			ctor: '::',
																			_0: {ctor: '_Tuple2', _0: 'Asia/Bishkek', _1: _elm_community$elm_time$Time_TimeZones$asia_bishkek},
																			_1: {
																				ctor: '::',
																				_0: {ctor: '_Tuple2', _0: 'Asia/Brunei', _1: _elm_community$elm_time$Time_TimeZones$asia_brunei},
																				_1: {
																					ctor: '::',
																					_0: {ctor: '_Tuple2', _0: 'Asia/Kolkata', _1: _elm_community$elm_time$Time_TimeZones$asia_calcutta},
																					_1: {
																						ctor: '::',
																						_0: {ctor: '_Tuple2', _0: 'Asia/Chita', _1: _elm_community$elm_time$Time_TimeZones$asia_chita},
																						_1: {
																							ctor: '::',
																							_0: {ctor: '_Tuple2', _0: 'Asia/Choibalsan', _1: _elm_community$elm_time$Time_TimeZones$asia_choibalsan},
																							_1: {
																								ctor: '::',
																								_0: {ctor: '_Tuple2', _0: 'Asia/Shanghai', _1: _elm_community$elm_time$Time_TimeZones$asia_chongqing},
																								_1: {
																									ctor: '::',
																									_0: {ctor: '_Tuple2', _0: 'Asia/Shanghai', _1: _elm_community$elm_time$Time_TimeZones$asia_chungking},
																									_1: {
																										ctor: '::',
																										_0: {ctor: '_Tuple2', _0: 'Asia/Colombo', _1: _elm_community$elm_time$Time_TimeZones$asia_colombo},
																										_1: {
																											ctor: '::',
																											_0: {ctor: '_Tuple2', _0: 'Asia/Dhaka', _1: _elm_community$elm_time$Time_TimeZones$asia_dacca},
																											_1: {
																												ctor: '::',
																												_0: {ctor: '_Tuple2', _0: 'Asia/Damascus', _1: _elm_community$elm_time$Time_TimeZones$asia_damascus},
																												_1: {
																													ctor: '::',
																													_0: {ctor: '_Tuple2', _0: 'Asia/Dhaka', _1: _elm_community$elm_time$Time_TimeZones$asia_dhaka},
																													_1: {
																														ctor: '::',
																														_0: {ctor: '_Tuple2', _0: 'Asia/Dili', _1: _elm_community$elm_time$Time_TimeZones$asia_dili},
																														_1: {
																															ctor: '::',
																															_0: {ctor: '_Tuple2', _0: 'Asia/Dubai', _1: _elm_community$elm_time$Time_TimeZones$asia_dubai},
																															_1: {
																																ctor: '::',
																																_0: {ctor: '_Tuple2', _0: 'Asia/Dushanbe', _1: _elm_community$elm_time$Time_TimeZones$asia_dushanbe},
																																_1: {
																																	ctor: '::',
																																	_0: {ctor: '_Tuple2', _0: 'Asia/Famagusta', _1: _elm_community$elm_time$Time_TimeZones$asia_famagusta},
																																	_1: {
																																		ctor: '::',
																																		_0: {ctor: '_Tuple2', _0: 'Asia/Gaza', _1: _elm_community$elm_time$Time_TimeZones$asia_gaza},
																																		_1: {
																																			ctor: '::',
																																			_0: {ctor: '_Tuple2', _0: 'Asia/Shanghai', _1: _elm_community$elm_time$Time_TimeZones$asia_harbin},
																																			_1: {
																																				ctor: '::',
																																				_0: {ctor: '_Tuple2', _0: 'Asia/Hebron', _1: _elm_community$elm_time$Time_TimeZones$asia_hebron},
																																				_1: {
																																					ctor: '::',
																																					_0: {ctor: '_Tuple2', _0: 'Asia/Ho_Chi_Minh', _1: _elm_community$elm_time$Time_TimeZones$asia_ho_chi_minh},
																																					_1: {
																																						ctor: '::',
																																						_0: {ctor: '_Tuple2', _0: 'Asia/Hong_Kong', _1: _elm_community$elm_time$Time_TimeZones$asia_hong_kong},
																																						_1: {
																																							ctor: '::',
																																							_0: {ctor: '_Tuple2', _0: 'Asia/Hovd', _1: _elm_community$elm_time$Time_TimeZones$asia_hovd},
																																							_1: {
																																								ctor: '::',
																																								_0: {ctor: '_Tuple2', _0: 'Asia/Irkutsk', _1: _elm_community$elm_time$Time_TimeZones$asia_irkutsk},
																																								_1: {
																																									ctor: '::',
																																									_0: {ctor: '_Tuple2', _0: 'Europe/Istanbul', _1: _elm_community$elm_time$Time_TimeZones$asia_istanbul},
																																									_1: {ctor: '[]'}
																																								}
																																							}
																																						}
																																					}
																																				}
																																			}
																																		}
																																	}
																																}
																															}
																														}
																													}
																												}
																											}
																										}
																									}
																								}
																							}
																						}
																					}
																				}
																			}
																		}
																	}
																}
															}
														}
													}
												}
											},
											_1: {
												ctor: '::',
												_0: {
													ctor: '::',
													_0: {ctor: '_Tuple2', _0: 'Asia/Jakarta', _1: _elm_community$elm_time$Time_TimeZones$asia_jakarta},
													_1: {
														ctor: '::',
														_0: {ctor: '_Tuple2', _0: 'Asia/Jayapura', _1: _elm_community$elm_time$Time_TimeZones$asia_jayapura},
														_1: {
															ctor: '::',
															_0: {ctor: '_Tuple2', _0: 'Asia/Jerusalem', _1: _elm_community$elm_time$Time_TimeZones$asia_jerusalem},
															_1: {
																ctor: '::',
																_0: {ctor: '_Tuple2', _0: 'Asia/Kabul', _1: _elm_community$elm_time$Time_TimeZones$asia_kabul},
																_1: {
																	ctor: '::',
																	_0: {ctor: '_Tuple2', _0: 'Asia/Kamchatka', _1: _elm_community$elm_time$Time_TimeZones$asia_kamchatka},
																	_1: {
																		ctor: '::',
																		_0: {ctor: '_Tuple2', _0: 'Asia/Karachi', _1: _elm_community$elm_time$Time_TimeZones$asia_karachi},
																		_1: {
																			ctor: '::',
																			_0: {ctor: '_Tuple2', _0: 'Asia/Urumqi', _1: _elm_community$elm_time$Time_TimeZones$asia_kashgar},
																			_1: {
																				ctor: '::',
																				_0: {ctor: '_Tuple2', _0: 'Asia/Kathmandu', _1: _elm_community$elm_time$Time_TimeZones$asia_kathmandu},
																				_1: {
																					ctor: '::',
																					_0: {ctor: '_Tuple2', _0: 'Asia/Kathmandu', _1: _elm_community$elm_time$Time_TimeZones$asia_katmandu},
																					_1: {
																						ctor: '::',
																						_0: {ctor: '_Tuple2', _0: 'Asia/Khandyga', _1: _elm_community$elm_time$Time_TimeZones$asia_khandyga},
																						_1: {
																							ctor: '::',
																							_0: {ctor: '_Tuple2', _0: 'Asia/Kolkata', _1: _elm_community$elm_time$Time_TimeZones$asia_kolkata},
																							_1: {
																								ctor: '::',
																								_0: {ctor: '_Tuple2', _0: 'Asia/Krasnoyarsk', _1: _elm_community$elm_time$Time_TimeZones$asia_krasnoyarsk},
																								_1: {
																									ctor: '::',
																									_0: {ctor: '_Tuple2', _0: 'Asia/Kuala_Lumpur', _1: _elm_community$elm_time$Time_TimeZones$asia_kuala_lumpur},
																									_1: {
																										ctor: '::',
																										_0: {ctor: '_Tuple2', _0: 'Asia/Kuching', _1: _elm_community$elm_time$Time_TimeZones$asia_kuching},
																										_1: {
																											ctor: '::',
																											_0: {ctor: '_Tuple2', _0: 'Asia/Riyadh', _1: _elm_community$elm_time$Time_TimeZones$asia_kuwait},
																											_1: {
																												ctor: '::',
																												_0: {ctor: '_Tuple2', _0: 'Asia/Macau', _1: _elm_community$elm_time$Time_TimeZones$asia_macao},
																												_1: {
																													ctor: '::',
																													_0: {ctor: '_Tuple2', _0: 'Asia/Macau', _1: _elm_community$elm_time$Time_TimeZones$asia_macau},
																													_1: {
																														ctor: '::',
																														_0: {ctor: '_Tuple2', _0: 'Asia/Magadan', _1: _elm_community$elm_time$Time_TimeZones$asia_magadan},
																														_1: {
																															ctor: '::',
																															_0: {ctor: '_Tuple2', _0: 'Asia/Makassar', _1: _elm_community$elm_time$Time_TimeZones$asia_makassar},
																															_1: {
																																ctor: '::',
																																_0: {ctor: '_Tuple2', _0: 'Asia/Manila', _1: _elm_community$elm_time$Time_TimeZones$asia_manila},
																																_1: {
																																	ctor: '::',
																																	_0: {ctor: '_Tuple2', _0: 'Asia/Dubai', _1: _elm_community$elm_time$Time_TimeZones$asia_muscat},
																																	_1: {
																																		ctor: '::',
																																		_0: {ctor: '_Tuple2', _0: 'Asia/Nicosia', _1: _elm_community$elm_time$Time_TimeZones$asia_nicosia},
																																		_1: {
																																			ctor: '::',
																																			_0: {ctor: '_Tuple2', _0: 'Asia/Novokuznetsk', _1: _elm_community$elm_time$Time_TimeZones$asia_novokuznetsk},
																																			_1: {
																																				ctor: '::',
																																				_0: {ctor: '_Tuple2', _0: 'Asia/Novosibirsk', _1: _elm_community$elm_time$Time_TimeZones$asia_novosibirsk},
																																				_1: {
																																					ctor: '::',
																																					_0: {ctor: '_Tuple2', _0: 'Asia/Omsk', _1: _elm_community$elm_time$Time_TimeZones$asia_omsk},
																																					_1: {
																																						ctor: '::',
																																						_0: {ctor: '_Tuple2', _0: 'Asia/Oral', _1: _elm_community$elm_time$Time_TimeZones$asia_oral},
																																						_1: {
																																							ctor: '::',
																																							_0: {ctor: '_Tuple2', _0: 'Asia/Bangkok', _1: _elm_community$elm_time$Time_TimeZones$asia_phnom_penh},
																																							_1: {
																																								ctor: '::',
																																								_0: {ctor: '_Tuple2', _0: 'Asia/Pontianak', _1: _elm_community$elm_time$Time_TimeZones$asia_pontianak},
																																								_1: {
																																									ctor: '::',
																																									_0: {ctor: '_Tuple2', _0: 'Asia/Pyongyang', _1: _elm_community$elm_time$Time_TimeZones$asia_pyongyang},
																																									_1: {
																																										ctor: '::',
																																										_0: {ctor: '_Tuple2', _0: 'Asia/Qatar', _1: _elm_community$elm_time$Time_TimeZones$asia_qatar},
																																										_1: {ctor: '[]'}
																																									}
																																								}
																																							}
																																						}
																																					}
																																				}
																																			}
																																		}
																																	}
																																}
																															}
																														}
																													}
																												}
																											}
																										}
																									}
																								}
																							}
																						}
																					}
																				}
																			}
																		}
																	}
																}
															}
														}
													}
												},
												_1: {
													ctor: '::',
													_0: {
														ctor: '::',
														_0: {ctor: '_Tuple2', _0: 'Asia/Qyzylorda', _1: _elm_community$elm_time$Time_TimeZones$asia_qyzylorda},
														_1: {
															ctor: '::',
															_0: {ctor: '_Tuple2', _0: 'Asia/Rangoon', _1: _elm_community$elm_time$Time_TimeZones$asia_rangoon},
															_1: {
																ctor: '::',
																_0: {ctor: '_Tuple2', _0: 'Asia/Riyadh', _1: _elm_community$elm_time$Time_TimeZones$asia_riyadh},
																_1: {
																	ctor: '::',
																	_0: {ctor: '_Tuple2', _0: 'Asia/Ho_Chi_Minh', _1: _elm_community$elm_time$Time_TimeZones$asia_saigon},
																	_1: {
																		ctor: '::',
																		_0: {ctor: '_Tuple2', _0: 'Asia/Sakhalin', _1: _elm_community$elm_time$Time_TimeZones$asia_sakhalin},
																		_1: {
																			ctor: '::',
																			_0: {ctor: '_Tuple2', _0: 'Asia/Samarkand', _1: _elm_community$elm_time$Time_TimeZones$asia_samarkand},
																			_1: {
																				ctor: '::',
																				_0: {ctor: '_Tuple2', _0: 'Asia/Seoul', _1: _elm_community$elm_time$Time_TimeZones$asia_seoul},
																				_1: {
																					ctor: '::',
																					_0: {ctor: '_Tuple2', _0: 'Asia/Shanghai', _1: _elm_community$elm_time$Time_TimeZones$asia_shanghai},
																					_1: {
																						ctor: '::',
																						_0: {ctor: '_Tuple2', _0: 'Asia/Singapore', _1: _elm_community$elm_time$Time_TimeZones$asia_singapore},
																						_1: {
																							ctor: '::',
																							_0: {ctor: '_Tuple2', _0: 'Asia/Srednekolymsk', _1: _elm_community$elm_time$Time_TimeZones$asia_srednekolymsk},
																							_1: {
																								ctor: '::',
																								_0: {ctor: '_Tuple2', _0: 'Asia/Taipei', _1: _elm_community$elm_time$Time_TimeZones$asia_taipei},
																								_1: {
																									ctor: '::',
																									_0: {ctor: '_Tuple2', _0: 'Asia/Tashkent', _1: _elm_community$elm_time$Time_TimeZones$asia_tashkent},
																									_1: {
																										ctor: '::',
																										_0: {ctor: '_Tuple2', _0: 'Asia/Tbilisi', _1: _elm_community$elm_time$Time_TimeZones$asia_tbilisi},
																										_1: {
																											ctor: '::',
																											_0: {ctor: '_Tuple2', _0: 'Asia/Tehran', _1: _elm_community$elm_time$Time_TimeZones$asia_tehran},
																											_1: {
																												ctor: '::',
																												_0: {ctor: '_Tuple2', _0: 'Asia/Jerusalem', _1: _elm_community$elm_time$Time_TimeZones$asia_tel_aviv},
																												_1: {
																													ctor: '::',
																													_0: {ctor: '_Tuple2', _0: 'Asia/Thimphu', _1: _elm_community$elm_time$Time_TimeZones$asia_thimbu},
																													_1: {
																														ctor: '::',
																														_0: {ctor: '_Tuple2', _0: 'Asia/Thimphu', _1: _elm_community$elm_time$Time_TimeZones$asia_thimphu},
																														_1: {
																															ctor: '::',
																															_0: {ctor: '_Tuple2', _0: 'Asia/Tokyo', _1: _elm_community$elm_time$Time_TimeZones$asia_tokyo},
																															_1: {
																																ctor: '::',
																																_0: {ctor: '_Tuple2', _0: 'Asia/Tomsk', _1: _elm_community$elm_time$Time_TimeZones$asia_tomsk},
																																_1: {
																																	ctor: '::',
																																	_0: {ctor: '_Tuple2', _0: 'Asia/Makassar', _1: _elm_community$elm_time$Time_TimeZones$asia_ujung_pandang},
																																	_1: {
																																		ctor: '::',
																																		_0: {ctor: '_Tuple2', _0: 'Asia/Ulaanbaatar', _1: _elm_community$elm_time$Time_TimeZones$asia_ulaanbaatar},
																																		_1: {
																																			ctor: '::',
																																			_0: {ctor: '_Tuple2', _0: 'Asia/Ulaanbaatar', _1: _elm_community$elm_time$Time_TimeZones$asia_ulan_bator},
																																			_1: {
																																				ctor: '::',
																																				_0: {ctor: '_Tuple2', _0: 'Asia/Urumqi', _1: _elm_community$elm_time$Time_TimeZones$asia_urumqi},
																																				_1: {
																																					ctor: '::',
																																					_0: {ctor: '_Tuple2', _0: 'Asia/Ust-Nera', _1: _elm_community$elm_time$Time_TimeZones$asia_ust_nera},
																																					_1: {
																																						ctor: '::',
																																						_0: {ctor: '_Tuple2', _0: 'Asia/Bangkok', _1: _elm_community$elm_time$Time_TimeZones$asia_vientiane},
																																						_1: {
																																							ctor: '::',
																																							_0: {ctor: '_Tuple2', _0: 'Asia/Vladivostok', _1: _elm_community$elm_time$Time_TimeZones$asia_vladivostok},
																																							_1: {
																																								ctor: '::',
																																								_0: {ctor: '_Tuple2', _0: 'Asia/Yakutsk', _1: _elm_community$elm_time$Time_TimeZones$asia_yakutsk},
																																								_1: {
																																									ctor: '::',
																																									_0: {ctor: '_Tuple2', _0: 'Asia/Rangoon', _1: _elm_community$elm_time$Time_TimeZones$asia_yangon},
																																									_1: {
																																										ctor: '::',
																																										_0: {ctor: '_Tuple2', _0: 'Asia/Yekaterinburg', _1: _elm_community$elm_time$Time_TimeZones$asia_yekaterinburg},
																																										_1: {
																																											ctor: '::',
																																											_0: {ctor: '_Tuple2', _0: 'Asia/Yerevan', _1: _elm_community$elm_time$Time_TimeZones$asia_yerevan},
																																											_1: {ctor: '[]'}
																																										}
																																									}
																																								}
																																							}
																																						}
																																					}
																																				}
																																			}
																																		}
																																	}
																																}
																															}
																														}
																													}
																												}
																											}
																										}
																									}
																								}
																							}
																						}
																					}
																				}
																			}
																		}
																	}
																}
															}
														}
													},
													_1: {
														ctor: '::',
														_0: {
															ctor: '::',
															_0: {ctor: '_Tuple2', _0: 'Atlantic/Azores', _1: _elm_community$elm_time$Time_TimeZones$atlantic_azores},
															_1: {
																ctor: '::',
																_0: {ctor: '_Tuple2', _0: 'Atlantic/Bermuda', _1: _elm_community$elm_time$Time_TimeZones$atlantic_bermuda},
																_1: {
																	ctor: '::',
																	_0: {ctor: '_Tuple2', _0: 'Atlantic/Canary', _1: _elm_community$elm_time$Time_TimeZones$atlantic_canary},
																	_1: {
																		ctor: '::',
																		_0: {ctor: '_Tuple2', _0: 'Atlantic/Cape_Verde', _1: _elm_community$elm_time$Time_TimeZones$atlantic_cape_verde},
																		_1: {
																			ctor: '::',
																			_0: {ctor: '_Tuple2', _0: 'Atlantic/Faroe', _1: _elm_community$elm_time$Time_TimeZones$atlantic_faeroe},
																			_1: {
																				ctor: '::',
																				_0: {ctor: '_Tuple2', _0: 'Atlantic/Faroe', _1: _elm_community$elm_time$Time_TimeZones$atlantic_faroe},
																				_1: {
																					ctor: '::',
																					_0: {ctor: '_Tuple2', _0: 'Europe/Oslo', _1: _elm_community$elm_time$Time_TimeZones$atlantic_jan_mayen},
																					_1: {
																						ctor: '::',
																						_0: {ctor: '_Tuple2', _0: 'Atlantic/Madeira', _1: _elm_community$elm_time$Time_TimeZones$atlantic_madeira},
																						_1: {
																							ctor: '::',
																							_0: {ctor: '_Tuple2', _0: 'Atlantic/Reykjavik', _1: _elm_community$elm_time$Time_TimeZones$atlantic_reykjavik},
																							_1: {
																								ctor: '::',
																								_0: {ctor: '_Tuple2', _0: 'Atlantic/South_Georgia', _1: _elm_community$elm_time$Time_TimeZones$atlantic_south_georgia},
																								_1: {
																									ctor: '::',
																									_0: {ctor: '_Tuple2', _0: 'Africa/Abidjan', _1: _elm_community$elm_time$Time_TimeZones$atlantic_st_helena},
																									_1: {
																										ctor: '::',
																										_0: {ctor: '_Tuple2', _0: 'Atlantic/Stanley', _1: _elm_community$elm_time$Time_TimeZones$atlantic_stanley},
																										_1: {
																											ctor: '::',
																											_0: {ctor: '_Tuple2', _0: 'Australia/Sydney', _1: _elm_community$elm_time$Time_TimeZones$australia_act},
																											_1: {
																												ctor: '::',
																												_0: {ctor: '_Tuple2', _0: 'Australia/Adelaide', _1: _elm_community$elm_time$Time_TimeZones$australia_adelaide},
																												_1: {
																													ctor: '::',
																													_0: {ctor: '_Tuple2', _0: 'Australia/Brisbane', _1: _elm_community$elm_time$Time_TimeZones$australia_brisbane},
																													_1: {
																														ctor: '::',
																														_0: {ctor: '_Tuple2', _0: 'Australia/Broken_Hill', _1: _elm_community$elm_time$Time_TimeZones$australia_broken_hill},
																														_1: {
																															ctor: '::',
																															_0: {ctor: '_Tuple2', _0: 'Australia/Sydney', _1: _elm_community$elm_time$Time_TimeZones$australia_canberra},
																															_1: {
																																ctor: '::',
																																_0: {ctor: '_Tuple2', _0: 'Australia/Currie', _1: _elm_community$elm_time$Time_TimeZones$australia_currie},
																																_1: {
																																	ctor: '::',
																																	_0: {ctor: '_Tuple2', _0: 'Australia/Darwin', _1: _elm_community$elm_time$Time_TimeZones$australia_darwin},
																																	_1: {
																																		ctor: '::',
																																		_0: {ctor: '_Tuple2', _0: 'Australia/Eucla', _1: _elm_community$elm_time$Time_TimeZones$australia_eucla},
																																		_1: {
																																			ctor: '::',
																																			_0: {ctor: '_Tuple2', _0: 'Australia/Hobart', _1: _elm_community$elm_time$Time_TimeZones$australia_hobart},
																																			_1: {
																																				ctor: '::',
																																				_0: {ctor: '_Tuple2', _0: 'Australia/Lord_Howe', _1: _elm_community$elm_time$Time_TimeZones$australia_lhi},
																																				_1: {
																																					ctor: '::',
																																					_0: {ctor: '_Tuple2', _0: 'Australia/Lindeman', _1: _elm_community$elm_time$Time_TimeZones$australia_lindeman},
																																					_1: {
																																						ctor: '::',
																																						_0: {ctor: '_Tuple2', _0: 'Australia/Lord_Howe', _1: _elm_community$elm_time$Time_TimeZones$australia_lord_howe},
																																						_1: {
																																							ctor: '::',
																																							_0: {ctor: '_Tuple2', _0: 'Australia/Melbourne', _1: _elm_community$elm_time$Time_TimeZones$australia_melbourne},
																																							_1: {
																																								ctor: '::',
																																								_0: {ctor: '_Tuple2', _0: 'Australia/Darwin', _1: _elm_community$elm_time$Time_TimeZones$australia_north},
																																								_1: {
																																									ctor: '::',
																																									_0: {ctor: '_Tuple2', _0: 'Australia/Sydney', _1: _elm_community$elm_time$Time_TimeZones$australia_nsw},
																																									_1: {
																																										ctor: '::',
																																										_0: {ctor: '_Tuple2', _0: 'Australia/Perth', _1: _elm_community$elm_time$Time_TimeZones$australia_perth},
																																										_1: {
																																											ctor: '::',
																																											_0: {ctor: '_Tuple2', _0: 'Australia/Brisbane', _1: _elm_community$elm_time$Time_TimeZones$australia_queensland},
																																											_1: {
																																												ctor: '::',
																																												_0: {ctor: '_Tuple2', _0: 'Australia/Adelaide', _1: _elm_community$elm_time$Time_TimeZones$australia_south},
																																												_1: {ctor: '[]'}
																																											}
																																										}
																																									}
																																								}
																																							}
																																						}
																																					}
																																				}
																																			}
																																		}
																																	}
																																}
																															}
																														}
																													}
																												}
																											}
																										}
																									}
																								}
																							}
																						}
																					}
																				}
																			}
																		}
																	}
																}
															}
														},
														_1: {
															ctor: '::',
															_0: {
																ctor: '::',
																_0: {ctor: '_Tuple2', _0: 'Australia/Sydney', _1: _elm_community$elm_time$Time_TimeZones$australia_sydney},
																_1: {
																	ctor: '::',
																	_0: {ctor: '_Tuple2', _0: 'Australia/Hobart', _1: _elm_community$elm_time$Time_TimeZones$australia_tasmania},
																	_1: {
																		ctor: '::',
																		_0: {ctor: '_Tuple2', _0: 'Australia/Melbourne', _1: _elm_community$elm_time$Time_TimeZones$australia_victoria},
																		_1: {
																			ctor: '::',
																			_0: {ctor: '_Tuple2', _0: 'Australia/Perth', _1: _elm_community$elm_time$Time_TimeZones$australia_west},
																			_1: {
																				ctor: '::',
																				_0: {ctor: '_Tuple2', _0: 'Australia/Broken_Hill', _1: _elm_community$elm_time$Time_TimeZones$australia_yancowinna},
																				_1: {
																					ctor: '::',
																					_0: {ctor: '_Tuple2', _0: 'America/Rio_Branco', _1: _elm_community$elm_time$Time_TimeZones$brazil_acre},
																					_1: {
																						ctor: '::',
																						_0: {ctor: '_Tuple2', _0: 'America/Noronha', _1: _elm_community$elm_time$Time_TimeZones$brazil_denoronha},
																						_1: {
																							ctor: '::',
																							_0: {ctor: '_Tuple2', _0: 'America/Sao_Paulo', _1: _elm_community$elm_time$Time_TimeZones$brazil_east},
																							_1: {
																								ctor: '::',
																								_0: {ctor: '_Tuple2', _0: 'America/Manaus', _1: _elm_community$elm_time$Time_TimeZones$brazil_west},
																								_1: {
																									ctor: '::',
																									_0: {ctor: '_Tuple2', _0: 'America/Halifax', _1: _elm_community$elm_time$Time_TimeZones$canada_atlantic},
																									_1: {
																										ctor: '::',
																										_0: {ctor: '_Tuple2', _0: 'America/Winnipeg', _1: _elm_community$elm_time$Time_TimeZones$canada_central},
																										_1: {
																											ctor: '::',
																											_0: {ctor: '_Tuple2', _0: 'America/Regina', _1: _elm_community$elm_time$Time_TimeZones$canada_east_saskatchewan},
																											_1: {
																												ctor: '::',
																												_0: {ctor: '_Tuple2', _0: 'America/Toronto', _1: _elm_community$elm_time$Time_TimeZones$canada_eastern},
																												_1: {
																													ctor: '::',
																													_0: {ctor: '_Tuple2', _0: 'America/Edmonton', _1: _elm_community$elm_time$Time_TimeZones$canada_mountain},
																													_1: {
																														ctor: '::',
																														_0: {ctor: '_Tuple2', _0: 'America/St_Johns', _1: _elm_community$elm_time$Time_TimeZones$canada_newfoundland},
																														_1: {
																															ctor: '::',
																															_0: {ctor: '_Tuple2', _0: 'America/Vancouver', _1: _elm_community$elm_time$Time_TimeZones$canada_pacific},
																															_1: {
																																ctor: '::',
																																_0: {ctor: '_Tuple2', _0: 'America/Regina', _1: _elm_community$elm_time$Time_TimeZones$canada_saskatchewan},
																																_1: {
																																	ctor: '::',
																																	_0: {ctor: '_Tuple2', _0: 'America/Whitehorse', _1: _elm_community$elm_time$Time_TimeZones$canada_yukon},
																																	_1: {
																																		ctor: '::',
																																		_0: {ctor: '_Tuple2', _0: 'CET', _1: _elm_community$elm_time$Time_TimeZones$cet},
																																		_1: {
																																			ctor: '::',
																																			_0: {ctor: '_Tuple2', _0: 'America/Santiago', _1: _elm_community$elm_time$Time_TimeZones$chile_continental},
																																			_1: {
																																				ctor: '::',
																																				_0: {ctor: '_Tuple2', _0: 'Pacific/Easter', _1: _elm_community$elm_time$Time_TimeZones$chile_easterisland},
																																				_1: {
																																					ctor: '::',
																																					_0: {ctor: '_Tuple2', _0: 'CST6CDT', _1: _elm_community$elm_time$Time_TimeZones$cst6cdt},
																																					_1: {
																																						ctor: '::',
																																						_0: {ctor: '_Tuple2', _0: 'America/Havana', _1: _elm_community$elm_time$Time_TimeZones$cuba},
																																						_1: {
																																							ctor: '::',
																																							_0: {ctor: '_Tuple2', _0: 'EET', _1: _elm_community$elm_time$Time_TimeZones$eet},
																																							_1: {
																																								ctor: '::',
																																								_0: {ctor: '_Tuple2', _0: 'Africa/Cairo', _1: _elm_community$elm_time$Time_TimeZones$egypt},
																																								_1: {
																																									ctor: '::',
																																									_0: {ctor: '_Tuple2', _0: 'Europe/Dublin', _1: _elm_community$elm_time$Time_TimeZones$eire},
																																									_1: {
																																										ctor: '::',
																																										_0: {ctor: '_Tuple2', _0: 'EST', _1: _elm_community$elm_time$Time_TimeZones$est},
																																										_1: {
																																											ctor: '::',
																																											_0: {ctor: '_Tuple2', _0: 'EST5EDT', _1: _elm_community$elm_time$Time_TimeZones$est5edt},
																																											_1: {
																																												ctor: '::',
																																												_0: {ctor: '_Tuple2', _0: 'Etc/GMT+0', _1: _elm_community$elm_time$Time_TimeZones$etc_gmt},
																																												_1: {
																																													ctor: '::',
																																													_0: {ctor: '_Tuple2', _0: 'Etc/GMT+0', _1: _elm_community$elm_time$Time_TimeZones$etc_gmt_0},
																																													_1: {ctor: '[]'}
																																												}
																																											}
																																										}
																																									}
																																								}
																																							}
																																						}
																																					}
																																				}
																																			}
																																		}
																																	}
																																}
																															}
																														}
																													}
																												}
																											}
																										}
																									}
																								}
																							}
																						}
																					}
																				}
																			}
																		}
																	}
																}
															},
															_1: {
																ctor: '::',
																_0: {
																	ctor: '::',
																	_0: {ctor: '_Tuple2', _0: 'Etc/GMT+0', _1: _elm_community$elm_time$Time_TimeZones$etc_gmt_minus_0},
																	_1: {
																		ctor: '::',
																		_0: {ctor: '_Tuple2', _0: 'Etc/GMT-1', _1: _elm_community$elm_time$Time_TimeZones$etc_gmt_minus_1},
																		_1: {
																			ctor: '::',
																			_0: {ctor: '_Tuple2', _0: 'Etc/GMT-10', _1: _elm_community$elm_time$Time_TimeZones$etc_gmt_minus_10},
																			_1: {
																				ctor: '::',
																				_0: {ctor: '_Tuple2', _0: 'Etc/GMT-11', _1: _elm_community$elm_time$Time_TimeZones$etc_gmt_minus_11},
																				_1: {
																					ctor: '::',
																					_0: {ctor: '_Tuple2', _0: 'Etc/GMT-12', _1: _elm_community$elm_time$Time_TimeZones$etc_gmt_minus_12},
																					_1: {
																						ctor: '::',
																						_0: {ctor: '_Tuple2', _0: 'Etc/GMT-13', _1: _elm_community$elm_time$Time_TimeZones$etc_gmt_minus_13},
																						_1: {
																							ctor: '::',
																							_0: {ctor: '_Tuple2', _0: 'Etc/GMT-14', _1: _elm_community$elm_time$Time_TimeZones$etc_gmt_minus_14},
																							_1: {
																								ctor: '::',
																								_0: {ctor: '_Tuple2', _0: 'Etc/GMT-2', _1: _elm_community$elm_time$Time_TimeZones$etc_gmt_minus_2},
																								_1: {
																									ctor: '::',
																									_0: {ctor: '_Tuple2', _0: 'Etc/GMT-3', _1: _elm_community$elm_time$Time_TimeZones$etc_gmt_minus_3},
																									_1: {
																										ctor: '::',
																										_0: {ctor: '_Tuple2', _0: 'Etc/GMT-4', _1: _elm_community$elm_time$Time_TimeZones$etc_gmt_minus_4},
																										_1: {
																											ctor: '::',
																											_0: {ctor: '_Tuple2', _0: 'Etc/GMT-5', _1: _elm_community$elm_time$Time_TimeZones$etc_gmt_minus_5},
																											_1: {
																												ctor: '::',
																												_0: {ctor: '_Tuple2', _0: 'Etc/GMT-6', _1: _elm_community$elm_time$Time_TimeZones$etc_gmt_minus_6},
																												_1: {
																													ctor: '::',
																													_0: {ctor: '_Tuple2', _0: 'Etc/GMT-7', _1: _elm_community$elm_time$Time_TimeZones$etc_gmt_minus_7},
																													_1: {
																														ctor: '::',
																														_0: {ctor: '_Tuple2', _0: 'Etc/GMT-8', _1: _elm_community$elm_time$Time_TimeZones$etc_gmt_minus_8},
																														_1: {
																															ctor: '::',
																															_0: {ctor: '_Tuple2', _0: 'Etc/GMT-9', _1: _elm_community$elm_time$Time_TimeZones$etc_gmt_minus_9},
																															_1: {
																																ctor: '::',
																																_0: {ctor: '_Tuple2', _0: 'Etc/GMT+0', _1: _elm_community$elm_time$Time_TimeZones$etc_gmt_plus_0},
																																_1: {
																																	ctor: '::',
																																	_0: {ctor: '_Tuple2', _0: 'Etc/GMT+1', _1: _elm_community$elm_time$Time_TimeZones$etc_gmt_plus_1},
																																	_1: {
																																		ctor: '::',
																																		_0: {ctor: '_Tuple2', _0: 'Etc/GMT+10', _1: _elm_community$elm_time$Time_TimeZones$etc_gmt_plus_10},
																																		_1: {
																																			ctor: '::',
																																			_0: {ctor: '_Tuple2', _0: 'Etc/GMT+11', _1: _elm_community$elm_time$Time_TimeZones$etc_gmt_plus_11},
																																			_1: {
																																				ctor: '::',
																																				_0: {ctor: '_Tuple2', _0: 'Etc/GMT+12', _1: _elm_community$elm_time$Time_TimeZones$etc_gmt_plus_12},
																																				_1: {
																																					ctor: '::',
																																					_0: {ctor: '_Tuple2', _0: 'Etc/GMT+2', _1: _elm_community$elm_time$Time_TimeZones$etc_gmt_plus_2},
																																					_1: {
																																						ctor: '::',
																																						_0: {ctor: '_Tuple2', _0: 'Etc/GMT+3', _1: _elm_community$elm_time$Time_TimeZones$etc_gmt_plus_3},
																																						_1: {
																																							ctor: '::',
																																							_0: {ctor: '_Tuple2', _0: 'Etc/GMT+4', _1: _elm_community$elm_time$Time_TimeZones$etc_gmt_plus_4},
																																							_1: {
																																								ctor: '::',
																																								_0: {ctor: '_Tuple2', _0: 'Etc/GMT+5', _1: _elm_community$elm_time$Time_TimeZones$etc_gmt_plus_5},
																																								_1: {
																																									ctor: '::',
																																									_0: {ctor: '_Tuple2', _0: 'Etc/GMT+6', _1: _elm_community$elm_time$Time_TimeZones$etc_gmt_plus_6},
																																									_1: {
																																										ctor: '::',
																																										_0: {ctor: '_Tuple2', _0: 'Etc/GMT+7', _1: _elm_community$elm_time$Time_TimeZones$etc_gmt_plus_7},
																																										_1: {
																																											ctor: '::',
																																											_0: {ctor: '_Tuple2', _0: 'Etc/GMT+8', _1: _elm_community$elm_time$Time_TimeZones$etc_gmt_plus_8},
																																											_1: {
																																												ctor: '::',
																																												_0: {ctor: '_Tuple2', _0: 'Etc/GMT+9', _1: _elm_community$elm_time$Time_TimeZones$etc_gmt_plus_9},
																																												_1: {
																																													ctor: '::',
																																													_0: {ctor: '_Tuple2', _0: 'Etc/GMT+0', _1: _elm_community$elm_time$Time_TimeZones$etc_greenwich},
																																													_1: {
																																														ctor: '::',
																																														_0: {ctor: '_Tuple2', _0: 'Etc/UCT', _1: _elm_community$elm_time$Time_TimeZones$etc_uct},
																																														_1: {ctor: '[]'}
																																													}
																																												}
																																											}
																																										}
																																									}
																																								}
																																							}
																																						}
																																					}
																																				}
																																			}
																																		}
																																	}
																																}
																															}
																														}
																													}
																												}
																											}
																										}
																									}
																								}
																							}
																						}
																					}
																				}
																			}
																		}
																	}
																},
																_1: {
																	ctor: '::',
																	_0: {
																		ctor: '::',
																		_0: {ctor: '_Tuple2', _0: 'Etc/UTC', _1: _elm_community$elm_time$Time_TimeZones$etc_universal},
																		_1: {
																			ctor: '::',
																			_0: {ctor: '_Tuple2', _0: 'Etc/UTC', _1: _elm_community$elm_time$Time_TimeZones$etc_utc},
																			_1: {
																				ctor: '::',
																				_0: {ctor: '_Tuple2', _0: 'Etc/UTC', _1: _elm_community$elm_time$Time_TimeZones$etc_zulu},
																				_1: {
																					ctor: '::',
																					_0: {ctor: '_Tuple2', _0: 'Europe/Amsterdam', _1: _elm_community$elm_time$Time_TimeZones$europe_amsterdam},
																					_1: {
																						ctor: '::',
																						_0: {ctor: '_Tuple2', _0: 'Europe/Andorra', _1: _elm_community$elm_time$Time_TimeZones$europe_andorra},
																						_1: {
																							ctor: '::',
																							_0: {ctor: '_Tuple2', _0: 'Europe/Astrakhan', _1: _elm_community$elm_time$Time_TimeZones$europe_astrakhan},
																							_1: {
																								ctor: '::',
																								_0: {ctor: '_Tuple2', _0: 'Europe/Athens', _1: _elm_community$elm_time$Time_TimeZones$europe_athens},
																								_1: {
																									ctor: '::',
																									_0: {ctor: '_Tuple2', _0: 'Europe/London', _1: _elm_community$elm_time$Time_TimeZones$europe_belfast},
																									_1: {
																										ctor: '::',
																										_0: {ctor: '_Tuple2', _0: 'Europe/Belgrade', _1: _elm_community$elm_time$Time_TimeZones$europe_belgrade},
																										_1: {
																											ctor: '::',
																											_0: {ctor: '_Tuple2', _0: 'Europe/Berlin', _1: _elm_community$elm_time$Time_TimeZones$europe_berlin},
																											_1: {
																												ctor: '::',
																												_0: {ctor: '_Tuple2', _0: 'Europe/Prague', _1: _elm_community$elm_time$Time_TimeZones$europe_bratislava},
																												_1: {
																													ctor: '::',
																													_0: {ctor: '_Tuple2', _0: 'Europe/Brussels', _1: _elm_community$elm_time$Time_TimeZones$europe_brussels},
																													_1: {
																														ctor: '::',
																														_0: {ctor: '_Tuple2', _0: 'Europe/Bucharest', _1: _elm_community$elm_time$Time_TimeZones$europe_bucharest},
																														_1: {
																															ctor: '::',
																															_0: {ctor: '_Tuple2', _0: 'Europe/Budapest', _1: _elm_community$elm_time$Time_TimeZones$europe_budapest},
																															_1: {
																																ctor: '::',
																																_0: {ctor: '_Tuple2', _0: 'Europe/Zurich', _1: _elm_community$elm_time$Time_TimeZones$europe_busingen},
																																_1: {
																																	ctor: '::',
																																	_0: {ctor: '_Tuple2', _0: 'Europe/Chisinau', _1: _elm_community$elm_time$Time_TimeZones$europe_chisinau},
																																	_1: {
																																		ctor: '::',
																																		_0: {ctor: '_Tuple2', _0: 'Europe/Copenhagen', _1: _elm_community$elm_time$Time_TimeZones$europe_copenhagen},
																																		_1: {
																																			ctor: '::',
																																			_0: {ctor: '_Tuple2', _0: 'Europe/Dublin', _1: _elm_community$elm_time$Time_TimeZones$europe_dublin},
																																			_1: {
																																				ctor: '::',
																																				_0: {ctor: '_Tuple2', _0: 'Europe/Gibraltar', _1: _elm_community$elm_time$Time_TimeZones$europe_gibraltar},
																																				_1: {
																																					ctor: '::',
																																					_0: {ctor: '_Tuple2', _0: 'Europe/London', _1: _elm_community$elm_time$Time_TimeZones$europe_guernsey},
																																					_1: {
																																						ctor: '::',
																																						_0: {ctor: '_Tuple2', _0: 'Europe/Helsinki', _1: _elm_community$elm_time$Time_TimeZones$europe_helsinki},
																																						_1: {
																																							ctor: '::',
																																							_0: {ctor: '_Tuple2', _0: 'Europe/London', _1: _elm_community$elm_time$Time_TimeZones$europe_isle_of_man},
																																							_1: {
																																								ctor: '::',
																																								_0: {ctor: '_Tuple2', _0: 'Europe/Istanbul', _1: _elm_community$elm_time$Time_TimeZones$europe_istanbul},
																																								_1: {
																																									ctor: '::',
																																									_0: {ctor: '_Tuple2', _0: 'Europe/London', _1: _elm_community$elm_time$Time_TimeZones$europe_jersey},
																																									_1: {
																																										ctor: '::',
																																										_0: {ctor: '_Tuple2', _0: 'Europe/Kaliningrad', _1: _elm_community$elm_time$Time_TimeZones$europe_kaliningrad},
																																										_1: {
																																											ctor: '::',
																																											_0: {ctor: '_Tuple2', _0: 'Europe/Kiev', _1: _elm_community$elm_time$Time_TimeZones$europe_kiev},
																																											_1: {
																																												ctor: '::',
																																												_0: {ctor: '_Tuple2', _0: 'Europe/Kirov', _1: _elm_community$elm_time$Time_TimeZones$europe_kirov},
																																												_1: {
																																													ctor: '::',
																																													_0: {ctor: '_Tuple2', _0: 'Europe/Lisbon', _1: _elm_community$elm_time$Time_TimeZones$europe_lisbon},
																																													_1: {
																																														ctor: '::',
																																														_0: {ctor: '_Tuple2', _0: 'Europe/Belgrade', _1: _elm_community$elm_time$Time_TimeZones$europe_ljubljana},
																																														_1: {
																																															ctor: '::',
																																															_0: {ctor: '_Tuple2', _0: 'Europe/London', _1: _elm_community$elm_time$Time_TimeZones$europe_london},
																																															_1: {ctor: '[]'}
																																														}
																																													}
																																												}
																																											}
																																										}
																																									}
																																								}
																																							}
																																						}
																																					}
																																				}
																																			}
																																		}
																																	}
																																}
																															}
																														}
																													}
																												}
																											}
																										}
																									}
																								}
																							}
																						}
																					}
																				}
																			}
																		}
																	},
																	_1: {
																		ctor: '::',
																		_0: {
																			ctor: '::',
																			_0: {ctor: '_Tuple2', _0: 'Europe/Luxembourg', _1: _elm_community$elm_time$Time_TimeZones$europe_luxembourg},
																			_1: {
																				ctor: '::',
																				_0: {ctor: '_Tuple2', _0: 'Europe/Madrid', _1: _elm_community$elm_time$Time_TimeZones$europe_madrid},
																				_1: {
																					ctor: '::',
																					_0: {ctor: '_Tuple2', _0: 'Europe/Malta', _1: _elm_community$elm_time$Time_TimeZones$europe_malta},
																					_1: {
																						ctor: '::',
																						_0: {ctor: '_Tuple2', _0: 'Europe/Helsinki', _1: _elm_community$elm_time$Time_TimeZones$europe_mariehamn},
																						_1: {
																							ctor: '::',
																							_0: {ctor: '_Tuple2', _0: 'Europe/Minsk', _1: _elm_community$elm_time$Time_TimeZones$europe_minsk},
																							_1: {
																								ctor: '::',
																								_0: {ctor: '_Tuple2', _0: 'Europe/Monaco', _1: _elm_community$elm_time$Time_TimeZones$europe_monaco},
																								_1: {
																									ctor: '::',
																									_0: {ctor: '_Tuple2', _0: 'Europe/Moscow', _1: _elm_community$elm_time$Time_TimeZones$europe_moscow},
																									_1: {
																										ctor: '::',
																										_0: {ctor: '_Tuple2', _0: 'Asia/Nicosia', _1: _elm_community$elm_time$Time_TimeZones$europe_nicosia},
																										_1: {
																											ctor: '::',
																											_0: {ctor: '_Tuple2', _0: 'Europe/Oslo', _1: _elm_community$elm_time$Time_TimeZones$europe_oslo},
																											_1: {
																												ctor: '::',
																												_0: {ctor: '_Tuple2', _0: 'Europe/Paris', _1: _elm_community$elm_time$Time_TimeZones$europe_paris},
																												_1: {
																													ctor: '::',
																													_0: {ctor: '_Tuple2', _0: 'Europe/Belgrade', _1: _elm_community$elm_time$Time_TimeZones$europe_podgorica},
																													_1: {
																														ctor: '::',
																														_0: {ctor: '_Tuple2', _0: 'Europe/Prague', _1: _elm_community$elm_time$Time_TimeZones$europe_prague},
																														_1: {
																															ctor: '::',
																															_0: {ctor: '_Tuple2', _0: 'Europe/Riga', _1: _elm_community$elm_time$Time_TimeZones$europe_riga},
																															_1: {
																																ctor: '::',
																																_0: {ctor: '_Tuple2', _0: 'Europe/Rome', _1: _elm_community$elm_time$Time_TimeZones$europe_rome},
																																_1: {
																																	ctor: '::',
																																	_0: {ctor: '_Tuple2', _0: 'Europe/Samara', _1: _elm_community$elm_time$Time_TimeZones$europe_samara},
																																	_1: {
																																		ctor: '::',
																																		_0: {ctor: '_Tuple2', _0: 'Europe/Rome', _1: _elm_community$elm_time$Time_TimeZones$europe_san_marino},
																																		_1: {
																																			ctor: '::',
																																			_0: {ctor: '_Tuple2', _0: 'Europe/Belgrade', _1: _elm_community$elm_time$Time_TimeZones$europe_sarajevo},
																																			_1: {
																																				ctor: '::',
																																				_0: {ctor: '_Tuple2', _0: 'Europe/Saratov', _1: _elm_community$elm_time$Time_TimeZones$europe_saratov},
																																				_1: {
																																					ctor: '::',
																																					_0: {ctor: '_Tuple2', _0: 'Europe/Simferopol', _1: _elm_community$elm_time$Time_TimeZones$europe_simferopol},
																																					_1: {
																																						ctor: '::',
																																						_0: {ctor: '_Tuple2', _0: 'Europe/Belgrade', _1: _elm_community$elm_time$Time_TimeZones$europe_skopje},
																																						_1: {
																																							ctor: '::',
																																							_0: {ctor: '_Tuple2', _0: 'Europe/Sofia', _1: _elm_community$elm_time$Time_TimeZones$europe_sofia},
																																							_1: {
																																								ctor: '::',
																																								_0: {ctor: '_Tuple2', _0: 'Europe/Stockholm', _1: _elm_community$elm_time$Time_TimeZones$europe_stockholm},
																																								_1: {
																																									ctor: '::',
																																									_0: {ctor: '_Tuple2', _0: 'Europe/Tallinn', _1: _elm_community$elm_time$Time_TimeZones$europe_tallinn},
																																									_1: {
																																										ctor: '::',
																																										_0: {ctor: '_Tuple2', _0: 'Europe/Tirane', _1: _elm_community$elm_time$Time_TimeZones$europe_tirane},
																																										_1: {
																																											ctor: '::',
																																											_0: {ctor: '_Tuple2', _0: 'Europe/Chisinau', _1: _elm_community$elm_time$Time_TimeZones$europe_tiraspol},
																																											_1: {
																																												ctor: '::',
																																												_0: {ctor: '_Tuple2', _0: 'Europe/Ulyanovsk', _1: _elm_community$elm_time$Time_TimeZones$europe_ulyanovsk},
																																												_1: {
																																													ctor: '::',
																																													_0: {ctor: '_Tuple2', _0: 'Europe/Uzhgorod', _1: _elm_community$elm_time$Time_TimeZones$europe_uzhgorod},
																																													_1: {
																																														ctor: '::',
																																														_0: {ctor: '_Tuple2', _0: 'Europe/Zurich', _1: _elm_community$elm_time$Time_TimeZones$europe_vaduz},
																																														_1: {
																																															ctor: '::',
																																															_0: {ctor: '_Tuple2', _0: 'Europe/Rome', _1: _elm_community$elm_time$Time_TimeZones$europe_vatican},
																																															_1: {
																																																ctor: '::',
																																																_0: {ctor: '_Tuple2', _0: 'Europe/Vienna', _1: _elm_community$elm_time$Time_TimeZones$europe_vienna},
																																																_1: {ctor: '[]'}
																																															}
																																														}
																																													}
																																												}
																																											}
																																										}
																																									}
																																								}
																																							}
																																						}
																																					}
																																				}
																																			}
																																		}
																																	}
																																}
																															}
																														}
																													}
																												}
																											}
																										}
																									}
																								}
																							}
																						}
																					}
																				}
																			}
																		},
																		_1: {
																			ctor: '::',
																			_0: {
																				ctor: '::',
																				_0: {ctor: '_Tuple2', _0: 'Europe/Vilnius', _1: _elm_community$elm_time$Time_TimeZones$europe_vilnius},
																				_1: {
																					ctor: '::',
																					_0: {ctor: '_Tuple2', _0: 'Europe/Volgograd', _1: _elm_community$elm_time$Time_TimeZones$europe_volgograd},
																					_1: {
																						ctor: '::',
																						_0: {ctor: '_Tuple2', _0: 'Europe/Warsaw', _1: _elm_community$elm_time$Time_TimeZones$europe_warsaw},
																						_1: {
																							ctor: '::',
																							_0: {ctor: '_Tuple2', _0: 'Europe/Belgrade', _1: _elm_community$elm_time$Time_TimeZones$europe_zagreb},
																							_1: {
																								ctor: '::',
																								_0: {ctor: '_Tuple2', _0: 'Europe/Zaporozhye', _1: _elm_community$elm_time$Time_TimeZones$europe_zaporozhye},
																								_1: {
																									ctor: '::',
																									_0: {ctor: '_Tuple2', _0: 'Europe/Zurich', _1: _elm_community$elm_time$Time_TimeZones$europe_zurich},
																									_1: {
																										ctor: '::',
																										_0: {ctor: '_Tuple2', _0: 'Europe/London', _1: _elm_community$elm_time$Time_TimeZones$gb},
																										_1: {
																											ctor: '::',
																											_0: {ctor: '_Tuple2', _0: 'Europe/London', _1: _elm_community$elm_time$Time_TimeZones$gb_eire},
																											_1: {
																												ctor: '::',
																												_0: {ctor: '_Tuple2', _0: 'Etc/GMT+0', _1: _elm_community$elm_time$Time_TimeZones$gmt},
																												_1: {
																													ctor: '::',
																													_0: {ctor: '_Tuple2', _0: 'Etc/GMT+0', _1: _elm_community$elm_time$Time_TimeZones$gmt_0},
																													_1: {
																														ctor: '::',
																														_0: {ctor: '_Tuple2', _0: 'Etc/GMT+0', _1: _elm_community$elm_time$Time_TimeZones$gmt_minus_0},
																														_1: {
																															ctor: '::',
																															_0: {ctor: '_Tuple2', _0: 'Etc/GMT+0', _1: _elm_community$elm_time$Time_TimeZones$gmt_plus_0},
																															_1: {
																																ctor: '::',
																																_0: {ctor: '_Tuple2', _0: 'Etc/GMT+0', _1: _elm_community$elm_time$Time_TimeZones$greenwich},
																																_1: {
																																	ctor: '::',
																																	_0: {ctor: '_Tuple2', _0: 'Asia/Hong_Kong', _1: _elm_community$elm_time$Time_TimeZones$hongkong},
																																	_1: {
																																		ctor: '::',
																																		_0: {ctor: '_Tuple2', _0: 'HST', _1: _elm_community$elm_time$Time_TimeZones$hst},
																																		_1: {
																																			ctor: '::',
																																			_0: {ctor: '_Tuple2', _0: 'Atlantic/Reykjavik', _1: _elm_community$elm_time$Time_TimeZones$iceland},
																																			_1: {
																																				ctor: '::',
																																				_0: {ctor: '_Tuple2', _0: 'Africa/Nairobi', _1: _elm_community$elm_time$Time_TimeZones$indian_antananarivo},
																																				_1: {
																																					ctor: '::',
																																					_0: {ctor: '_Tuple2', _0: 'Indian/Chagos', _1: _elm_community$elm_time$Time_TimeZones$indian_chagos},
																																					_1: {
																																						ctor: '::',
																																						_0: {ctor: '_Tuple2', _0: 'Indian/Christmas', _1: _elm_community$elm_time$Time_TimeZones$indian_christmas},
																																						_1: {
																																							ctor: '::',
																																							_0: {ctor: '_Tuple2', _0: 'Indian/Cocos', _1: _elm_community$elm_time$Time_TimeZones$indian_cocos},
																																							_1: {
																																								ctor: '::',
																																								_0: {ctor: '_Tuple2', _0: 'Africa/Nairobi', _1: _elm_community$elm_time$Time_TimeZones$indian_comoro},
																																								_1: {
																																									ctor: '::',
																																									_0: {ctor: '_Tuple2', _0: 'Indian/Kerguelen', _1: _elm_community$elm_time$Time_TimeZones$indian_kerguelen},
																																									_1: {
																																										ctor: '::',
																																										_0: {ctor: '_Tuple2', _0: 'Indian/Mahe', _1: _elm_community$elm_time$Time_TimeZones$indian_mahe},
																																										_1: {
																																											ctor: '::',
																																											_0: {ctor: '_Tuple2', _0: 'Indian/Maldives', _1: _elm_community$elm_time$Time_TimeZones$indian_maldives},
																																											_1: {
																																												ctor: '::',
																																												_0: {ctor: '_Tuple2', _0: 'Indian/Mauritius', _1: _elm_community$elm_time$Time_TimeZones$indian_mauritius},
																																												_1: {
																																													ctor: '::',
																																													_0: {ctor: '_Tuple2', _0: 'Africa/Nairobi', _1: _elm_community$elm_time$Time_TimeZones$indian_mayotte},
																																													_1: {
																																														ctor: '::',
																																														_0: {ctor: '_Tuple2', _0: 'Indian/Reunion', _1: _elm_community$elm_time$Time_TimeZones$indian_reunion},
																																														_1: {
																																															ctor: '::',
																																															_0: {ctor: '_Tuple2', _0: 'Asia/Tehran', _1: _elm_community$elm_time$Time_TimeZones$iran},
																																															_1: {
																																																ctor: '::',
																																																_0: {ctor: '_Tuple2', _0: 'Asia/Jerusalem', _1: _elm_community$elm_time$Time_TimeZones$israel},
																																																_1: {
																																																	ctor: '::',
																																																	_0: {ctor: '_Tuple2', _0: 'America/Jamaica', _1: _elm_community$elm_time$Time_TimeZones$jamaica},
																																																	_1: {ctor: '[]'}
																																																}
																																															}
																																														}
																																													}
																																												}
																																											}
																																										}
																																									}
																																								}
																																							}
																																						}
																																					}
																																				}
																																			}
																																		}
																																	}
																																}
																															}
																														}
																													}
																												}
																											}
																										}
																									}
																								}
																							}
																						}
																					}
																				}
																			},
																			_1: {
																				ctor: '::',
																				_0: {
																					ctor: '::',
																					_0: {ctor: '_Tuple2', _0: 'Asia/Tokyo', _1: _elm_community$elm_time$Time_TimeZones$japan},
																					_1: {
																						ctor: '::',
																						_0: {ctor: '_Tuple2', _0: 'Pacific/Kwajalein', _1: _elm_community$elm_time$Time_TimeZones$kwajalein},
																						_1: {
																							ctor: '::',
																							_0: {ctor: '_Tuple2', _0: 'Africa/Tripoli', _1: _elm_community$elm_time$Time_TimeZones$libya},
																							_1: {
																								ctor: '::',
																								_0: {ctor: '_Tuple2', _0: 'MET', _1: _elm_community$elm_time$Time_TimeZones$met},
																								_1: {
																									ctor: '::',
																									_0: {ctor: '_Tuple2', _0: 'America/Tijuana', _1: _elm_community$elm_time$Time_TimeZones$mexico_bajanorte},
																									_1: {
																										ctor: '::',
																										_0: {ctor: '_Tuple2', _0: 'America/Mazatlan', _1: _elm_community$elm_time$Time_TimeZones$mexico_bajasur},
																										_1: {
																											ctor: '::',
																											_0: {ctor: '_Tuple2', _0: 'America/Mexico_City', _1: _elm_community$elm_time$Time_TimeZones$mexico_general},
																											_1: {
																												ctor: '::',
																												_0: {ctor: '_Tuple2', _0: 'MST', _1: _elm_community$elm_time$Time_TimeZones$mst},
																												_1: {
																													ctor: '::',
																													_0: {ctor: '_Tuple2', _0: 'MST7MDT', _1: _elm_community$elm_time$Time_TimeZones$mst7mdt},
																													_1: {
																														ctor: '::',
																														_0: {ctor: '_Tuple2', _0: 'America/Denver', _1: _elm_community$elm_time$Time_TimeZones$navajo},
																														_1: {
																															ctor: '::',
																															_0: {ctor: '_Tuple2', _0: 'Pacific/Auckland', _1: _elm_community$elm_time$Time_TimeZones$nz},
																															_1: {
																																ctor: '::',
																																_0: {ctor: '_Tuple2', _0: 'Pacific/Chatham', _1: _elm_community$elm_time$Time_TimeZones$nz_chat},
																																_1: {
																																	ctor: '::',
																																	_0: {ctor: '_Tuple2', _0: 'Pacific/Apia', _1: _elm_community$elm_time$Time_TimeZones$pacific_apia},
																																	_1: {
																																		ctor: '::',
																																		_0: {ctor: '_Tuple2', _0: 'Pacific/Auckland', _1: _elm_community$elm_time$Time_TimeZones$pacific_auckland},
																																		_1: {
																																			ctor: '::',
																																			_0: {ctor: '_Tuple2', _0: 'Pacific/Bougainville', _1: _elm_community$elm_time$Time_TimeZones$pacific_bougainville},
																																			_1: {
																																				ctor: '::',
																																				_0: {ctor: '_Tuple2', _0: 'Pacific/Chatham', _1: _elm_community$elm_time$Time_TimeZones$pacific_chatham},
																																				_1: {
																																					ctor: '::',
																																					_0: {ctor: '_Tuple2', _0: 'Pacific/Chuuk', _1: _elm_community$elm_time$Time_TimeZones$pacific_chuuk},
																																					_1: {
																																						ctor: '::',
																																						_0: {ctor: '_Tuple2', _0: 'Pacific/Easter', _1: _elm_community$elm_time$Time_TimeZones$pacific_easter},
																																						_1: {
																																							ctor: '::',
																																							_0: {ctor: '_Tuple2', _0: 'Pacific/Efate', _1: _elm_community$elm_time$Time_TimeZones$pacific_efate},
																																							_1: {
																																								ctor: '::',
																																								_0: {ctor: '_Tuple2', _0: 'Pacific/Enderbury', _1: _elm_community$elm_time$Time_TimeZones$pacific_enderbury},
																																								_1: {
																																									ctor: '::',
																																									_0: {ctor: '_Tuple2', _0: 'Pacific/Fakaofo', _1: _elm_community$elm_time$Time_TimeZones$pacific_fakaofo},
																																									_1: {
																																										ctor: '::',
																																										_0: {ctor: '_Tuple2', _0: 'Pacific/Fiji', _1: _elm_community$elm_time$Time_TimeZones$pacific_fiji},
																																										_1: {
																																											ctor: '::',
																																											_0: {ctor: '_Tuple2', _0: 'Pacific/Funafuti', _1: _elm_community$elm_time$Time_TimeZones$pacific_funafuti},
																																											_1: {
																																												ctor: '::',
																																												_0: {ctor: '_Tuple2', _0: 'Pacific/Galapagos', _1: _elm_community$elm_time$Time_TimeZones$pacific_galapagos},
																																												_1: {
																																													ctor: '::',
																																													_0: {ctor: '_Tuple2', _0: 'Pacific/Gambier', _1: _elm_community$elm_time$Time_TimeZones$pacific_gambier},
																																													_1: {
																																														ctor: '::',
																																														_0: {ctor: '_Tuple2', _0: 'Pacific/Guadalcanal', _1: _elm_community$elm_time$Time_TimeZones$pacific_guadalcanal},
																																														_1: {
																																															ctor: '::',
																																															_0: {ctor: '_Tuple2', _0: 'Pacific/Guam', _1: _elm_community$elm_time$Time_TimeZones$pacific_guam},
																																															_1: {
																																																ctor: '::',
																																																_0: {ctor: '_Tuple2', _0: 'Pacific/Honolulu', _1: _elm_community$elm_time$Time_TimeZones$pacific_honolulu},
																																																_1: {
																																																	ctor: '::',
																																																	_0: {ctor: '_Tuple2', _0: 'Pacific/Honolulu', _1: _elm_community$elm_time$Time_TimeZones$pacific_johnston},
																																																	_1: {
																																																		ctor: '::',
																																																		_0: {ctor: '_Tuple2', _0: 'Pacific/Kiritimati', _1: _elm_community$elm_time$Time_TimeZones$pacific_kiritimati},
																																																		_1: {ctor: '[]'}
																																																	}
																																																}
																																															}
																																														}
																																													}
																																												}
																																											}
																																										}
																																									}
																																								}
																																							}
																																						}
																																					}
																																				}
																																			}
																																		}
																																	}
																																}
																															}
																														}
																													}
																												}
																											}
																										}
																									}
																								}
																							}
																						}
																					}
																				},
																				_1: {
																					ctor: '::',
																					_0: {
																						ctor: '::',
																						_0: {ctor: '_Tuple2', _0: 'Pacific/Kosrae', _1: _elm_community$elm_time$Time_TimeZones$pacific_kosrae},
																						_1: {
																							ctor: '::',
																							_0: {ctor: '_Tuple2', _0: 'Pacific/Kwajalein', _1: _elm_community$elm_time$Time_TimeZones$pacific_kwajalein},
																							_1: {
																								ctor: '::',
																								_0: {ctor: '_Tuple2', _0: 'Pacific/Majuro', _1: _elm_community$elm_time$Time_TimeZones$pacific_majuro},
																								_1: {
																									ctor: '::',
																									_0: {ctor: '_Tuple2', _0: 'Pacific/Marquesas', _1: _elm_community$elm_time$Time_TimeZones$pacific_marquesas},
																									_1: {
																										ctor: '::',
																										_0: {ctor: '_Tuple2', _0: 'Pacific/Pago_Pago', _1: _elm_community$elm_time$Time_TimeZones$pacific_midway},
																										_1: {
																											ctor: '::',
																											_0: {ctor: '_Tuple2', _0: 'Pacific/Nauru', _1: _elm_community$elm_time$Time_TimeZones$pacific_nauru},
																											_1: {
																												ctor: '::',
																												_0: {ctor: '_Tuple2', _0: 'Pacific/Niue', _1: _elm_community$elm_time$Time_TimeZones$pacific_niue},
																												_1: {
																													ctor: '::',
																													_0: {ctor: '_Tuple2', _0: 'Pacific/Norfolk', _1: _elm_community$elm_time$Time_TimeZones$pacific_norfolk},
																													_1: {
																														ctor: '::',
																														_0: {ctor: '_Tuple2', _0: 'Pacific/Noumea', _1: _elm_community$elm_time$Time_TimeZones$pacific_noumea},
																														_1: {
																															ctor: '::',
																															_0: {ctor: '_Tuple2', _0: 'Pacific/Pago_Pago', _1: _elm_community$elm_time$Time_TimeZones$pacific_pago_pago},
																															_1: {
																																ctor: '::',
																																_0: {ctor: '_Tuple2', _0: 'Pacific/Palau', _1: _elm_community$elm_time$Time_TimeZones$pacific_palau},
																																_1: {
																																	ctor: '::',
																																	_0: {ctor: '_Tuple2', _0: 'Pacific/Pitcairn', _1: _elm_community$elm_time$Time_TimeZones$pacific_pitcairn},
																																	_1: {
																																		ctor: '::',
																																		_0: {ctor: '_Tuple2', _0: 'Pacific/Pohnpei', _1: _elm_community$elm_time$Time_TimeZones$pacific_pohnpei},
																																		_1: {
																																			ctor: '::',
																																			_0: {ctor: '_Tuple2', _0: 'Pacific/Pohnpei', _1: _elm_community$elm_time$Time_TimeZones$pacific_ponape},
																																			_1: {
																																				ctor: '::',
																																				_0: {ctor: '_Tuple2', _0: 'Pacific/Port_Moresby', _1: _elm_community$elm_time$Time_TimeZones$pacific_port_moresby},
																																				_1: {
																																					ctor: '::',
																																					_0: {ctor: '_Tuple2', _0: 'Pacific/Rarotonga', _1: _elm_community$elm_time$Time_TimeZones$pacific_rarotonga},
																																					_1: {
																																						ctor: '::',
																																						_0: {ctor: '_Tuple2', _0: 'Pacific/Guam', _1: _elm_community$elm_time$Time_TimeZones$pacific_saipan},
																																						_1: {
																																							ctor: '::',
																																							_0: {ctor: '_Tuple2', _0: 'Pacific/Pago_Pago', _1: _elm_community$elm_time$Time_TimeZones$pacific_samoa},
																																							_1: {
																																								ctor: '::',
																																								_0: {ctor: '_Tuple2', _0: 'Pacific/Tahiti', _1: _elm_community$elm_time$Time_TimeZones$pacific_tahiti},
																																								_1: {
																																									ctor: '::',
																																									_0: {ctor: '_Tuple2', _0: 'Pacific/Tarawa', _1: _elm_community$elm_time$Time_TimeZones$pacific_tarawa},
																																									_1: {
																																										ctor: '::',
																																										_0: {ctor: '_Tuple2', _0: 'Pacific/Tongatapu', _1: _elm_community$elm_time$Time_TimeZones$pacific_tongatapu},
																																										_1: {
																																											ctor: '::',
																																											_0: {ctor: '_Tuple2', _0: 'Pacific/Chuuk', _1: _elm_community$elm_time$Time_TimeZones$pacific_truk},
																																											_1: {
																																												ctor: '::',
																																												_0: {ctor: '_Tuple2', _0: 'Pacific/Wake', _1: _elm_community$elm_time$Time_TimeZones$pacific_wake},
																																												_1: {
																																													ctor: '::',
																																													_0: {ctor: '_Tuple2', _0: 'Pacific/Wallis', _1: _elm_community$elm_time$Time_TimeZones$pacific_wallis},
																																													_1: {
																																														ctor: '::',
																																														_0: {ctor: '_Tuple2', _0: 'Pacific/Chuuk', _1: _elm_community$elm_time$Time_TimeZones$pacific_yap},
																																														_1: {
																																															ctor: '::',
																																															_0: {ctor: '_Tuple2', _0: 'Europe/Warsaw', _1: _elm_community$elm_time$Time_TimeZones$poland},
																																															_1: {
																																																ctor: '::',
																																																_0: {ctor: '_Tuple2', _0: 'Europe/Lisbon', _1: _elm_community$elm_time$Time_TimeZones$portugal},
																																																_1: {
																																																	ctor: '::',
																																																	_0: {ctor: '_Tuple2', _0: 'Asia/Shanghai', _1: _elm_community$elm_time$Time_TimeZones$prc},
																																																	_1: {
																																																		ctor: '::',
																																																		_0: {ctor: '_Tuple2', _0: 'PST8PDT', _1: _elm_community$elm_time$Time_TimeZones$pst8pdt},
																																																		_1: {
																																																			ctor: '::',
																																																			_0: {ctor: '_Tuple2', _0: 'Asia/Taipei', _1: _elm_community$elm_time$Time_TimeZones$roc},
																																																			_1: {ctor: '[]'}
																																																		}
																																																	}
																																																}
																																															}
																																														}
																																													}
																																												}
																																											}
																																										}
																																									}
																																								}
																																							}
																																						}
																																					}
																																				}
																																			}
																																		}
																																	}
																																}
																															}
																														}
																													}
																												}
																											}
																										}
																									}
																								}
																							}
																						}
																					},
																					_1: {
																						ctor: '::',
																						_0: {
																							ctor: '::',
																							_0: {ctor: '_Tuple2', _0: 'Asia/Seoul', _1: _elm_community$elm_time$Time_TimeZones$rok},
																							_1: {
																								ctor: '::',
																								_0: {ctor: '_Tuple2', _0: 'Asia/Singapore', _1: _elm_community$elm_time$Time_TimeZones$singapore},
																								_1: {
																									ctor: '::',
																									_0: {ctor: '_Tuple2', _0: 'Europe/Istanbul', _1: _elm_community$elm_time$Time_TimeZones$turkey},
																									_1: {
																										ctor: '::',
																										_0: {ctor: '_Tuple2', _0: 'Etc/UCT', _1: _elm_community$elm_time$Time_TimeZones$uct},
																										_1: {
																											ctor: '::',
																											_0: {ctor: '_Tuple2', _0: 'Etc/UTC', _1: _elm_community$elm_time$Time_TimeZones$universal},
																											_1: {
																												ctor: '::',
																												_0: {ctor: '_Tuple2', _0: 'America/Anchorage', _1: _elm_community$elm_time$Time_TimeZones$us_alaska},
																												_1: {
																													ctor: '::',
																													_0: {ctor: '_Tuple2', _0: 'America/Adak', _1: _elm_community$elm_time$Time_TimeZones$us_aleutian},
																													_1: {
																														ctor: '::',
																														_0: {ctor: '_Tuple2', _0: 'America/Phoenix', _1: _elm_community$elm_time$Time_TimeZones$us_arizona},
																														_1: {
																															ctor: '::',
																															_0: {ctor: '_Tuple2', _0: 'America/Chicago', _1: _elm_community$elm_time$Time_TimeZones$us_central},
																															_1: {
																																ctor: '::',
																																_0: {ctor: '_Tuple2', _0: 'America/Fort_Wayne', _1: _elm_community$elm_time$Time_TimeZones$us_east_indiana},
																																_1: {
																																	ctor: '::',
																																	_0: {ctor: '_Tuple2', _0: 'America/New_York', _1: _elm_community$elm_time$Time_TimeZones$us_eastern},
																																	_1: {
																																		ctor: '::',
																																		_0: {ctor: '_Tuple2', _0: 'Pacific/Honolulu', _1: _elm_community$elm_time$Time_TimeZones$us_hawaii},
																																		_1: {
																																			ctor: '::',
																																			_0: {ctor: '_Tuple2', _0: 'America/Indiana/Knox', _1: _elm_community$elm_time$Time_TimeZones$us_indiana_starke},
																																			_1: {
																																				ctor: '::',
																																				_0: {ctor: '_Tuple2', _0: 'America/Detroit', _1: _elm_community$elm_time$Time_TimeZones$us_michigan},
																																				_1: {
																																					ctor: '::',
																																					_0: {ctor: '_Tuple2', _0: 'America/Denver', _1: _elm_community$elm_time$Time_TimeZones$us_mountain},
																																					_1: {
																																						ctor: '::',
																																						_0: {ctor: '_Tuple2', _0: 'America/Los_Angeles', _1: _elm_community$elm_time$Time_TimeZones$us_pacific},
																																						_1: {
																																							ctor: '::',
																																							_0: {ctor: '_Tuple2', _0: 'America/Los_Angeles', _1: _elm_community$elm_time$Time_TimeZones$us_pacific_new},
																																							_1: {
																																								ctor: '::',
																																								_0: {ctor: '_Tuple2', _0: 'Pacific/Pago_Pago', _1: _elm_community$elm_time$Time_TimeZones$us_samoa},
																																								_1: {
																																									ctor: '::',
																																									_0: {ctor: '_Tuple2', _0: 'Etc/UTC', _1: _elm_community$elm_time$Time_TimeZones$utc},
																																									_1: {
																																										ctor: '::',
																																										_0: {ctor: '_Tuple2', _0: 'Europe/Moscow', _1: _elm_community$elm_time$Time_TimeZones$w_su},
																																										_1: {
																																											ctor: '::',
																																											_0: {ctor: '_Tuple2', _0: 'WET', _1: _elm_community$elm_time$Time_TimeZones$wet},
																																											_1: {
																																												ctor: '::',
																																												_0: {ctor: '_Tuple2', _0: 'Etc/UTC', _1: _elm_community$elm_time$Time_TimeZones$zulu},
																																												_1: {ctor: '[]'}
																																											}
																																										}
																																									}
																																								}
																																							}
																																						}
																																					}
																																				}
																																			}
																																		}
																																	}
																																}
																															}
																														}
																													}
																												}
																											}
																										}
																									}
																								}
																							}
																						},
																						_1: {ctor: '[]'}
																					}
																				}
																			}
																		}
																	}
																}
															}
														}
													}
												}
											}
										}
									}
								}
							}
						}
					}
				}
			}
		}));
var _elm_community$elm_time$Time_TimeZones$fromName = function (name) {
	var _p1184 = A2(_elm_lang$core$Dict$get, name, _elm_community$elm_time$Time_TimeZones$all);
	if (_p1184.ctor === 'Nothing') {
		return _elm_lang$core$Maybe$Nothing;
	} else {
		return _elm_lang$core$Maybe$Just(
			_p1184._0(
				{ctor: '_Tuple0'}));
	}
};

var _elm_community$elm_time$Time_ZonedDateTime$millisecond = function (_p0) {
	var _p1 = _p0;
	return _elm_community$elm_time$Time_DateTime$millisecond(_p1._0.dateTime);
};
var _elm_community$elm_time$Time_ZonedDateTime$second = function (_p2) {
	var _p3 = _p2;
	return _elm_community$elm_time$Time_DateTime$second(_p3._0.dateTime);
};
var _elm_community$elm_time$Time_ZonedDateTime$minute = function (_p4) {
	var _p5 = _p4;
	return _elm_community$elm_time$Time_DateTime$minute(_p5._0.dateTime);
};
var _elm_community$elm_time$Time_ZonedDateTime$hour = function (_p6) {
	var _p7 = _p6;
	return _elm_community$elm_time$Time_DateTime$hour(_p7._0.dateTime);
};
var _elm_community$elm_time$Time_ZonedDateTime$weekday = function (_p8) {
	var _p9 = _p8;
	return _elm_community$elm_time$Time_DateTime$weekday(_p9._0.dateTime);
};
var _elm_community$elm_time$Time_ZonedDateTime$day = function (_p10) {
	var _p11 = _p10;
	return _elm_community$elm_time$Time_DateTime$day(_p11._0.dateTime);
};
var _elm_community$elm_time$Time_ZonedDateTime$month = function (_p12) {
	var _p13 = _p12;
	return _elm_community$elm_time$Time_DateTime$month(_p13._0.dateTime);
};
var _elm_community$elm_time$Time_ZonedDateTime$year = function (_p14) {
	var _p15 = _p14;
	return _elm_community$elm_time$Time_DateTime$year(_p15._0.dateTime);
};
var _elm_community$elm_time$Time_ZonedDateTime$timeZone = function (_p16) {
	var _p17 = _p16;
	return _p17._0.timeZone;
};
var _elm_community$elm_time$Time_ZonedDateTime$toTimestamp = function (_p18) {
	var _p19 = _p18;
	var _p20 = _p19._0.dateTime;
	return _elm_community$elm_time$Time_DateTime$toTimestamp(
		A3(
			_elm_lang$core$Basics$flip,
			_elm_community$elm_time$Time_DateTime$addMilliseconds,
			_p20,
			A3(
				_elm_lang$core$Basics$flip,
				_elm_community$elm_time$Time_TimeZone$offset,
				_p19._0.timeZone,
				_elm_community$elm_time$Time_DateTime$toTimestamp(_p20))));
};
var _elm_community$elm_time$Time_ZonedDateTime$abbreviation = function (_p21) {
	var _p22 = _p21;
	return A3(
		_elm_lang$core$Basics$flip,
		_elm_community$elm_time$Time_TimeZone$abbreviation,
		_p22._0.timeZone,
		_elm_community$elm_time$Time_ZonedDateTime$toTimestamp(_p22));
};
var _elm_community$elm_time$Time_ZonedDateTime$utcOffset = function (_p23) {
	var _p24 = _p23;
	return A3(
		_elm_lang$core$Basics$flip,
		_elm_community$elm_time$Time_TimeZone$offset,
		_p24._0.timeZone,
		_elm_community$elm_time$Time_ZonedDateTime$toTimestamp(_p24));
};
var _elm_community$elm_time$Time_ZonedDateTime$utcOffsetString = function (_p25) {
	var _p26 = _p25;
	return A3(
		_elm_lang$core$Basics$flip,
		_elm_community$elm_time$Time_TimeZone$offsetString,
		_p26._0.timeZone,
		_elm_community$elm_time$Time_ZonedDateTime$toTimestamp(_p26));
};
var _elm_community$elm_time$Time_ZonedDateTime$toISO8601 = function (dateTime) {
	return A2(
		_elm_lang$core$Basics_ops['++'],
		_elm_lang$core$Basics$toString(
			_elm_community$elm_time$Time_ZonedDateTime$year(dateTime)),
		A2(
			_elm_lang$core$Basics_ops['++'],
			'-',
			A2(
				_elm_lang$core$Basics_ops['++'],
				_elm_community$elm_time$Time_Internal$padded(
					_elm_community$elm_time$Time_ZonedDateTime$month(dateTime)),
				A2(
					_elm_lang$core$Basics_ops['++'],
					'-',
					A2(
						_elm_lang$core$Basics_ops['++'],
						_elm_community$elm_time$Time_Internal$padded(
							_elm_community$elm_time$Time_ZonedDateTime$day(dateTime)),
						A2(
							_elm_lang$core$Basics_ops['++'],
							'T',
							A2(
								_elm_lang$core$Basics_ops['++'],
								_elm_community$elm_time$Time_Internal$padded(
									_elm_community$elm_time$Time_ZonedDateTime$hour(dateTime)),
								A2(
									_elm_lang$core$Basics_ops['++'],
									':',
									A2(
										_elm_lang$core$Basics_ops['++'],
										_elm_community$elm_time$Time_Internal$padded(
											_elm_community$elm_time$Time_ZonedDateTime$minute(dateTime)),
										A2(
											_elm_lang$core$Basics_ops['++'],
											':',
											A2(
												_elm_lang$core$Basics_ops['++'],
												_elm_community$elm_time$Time_Internal$padded(
													_elm_community$elm_time$Time_ZonedDateTime$second(dateTime)),
												A2(
													_elm_lang$core$Basics_ops['++'],
													'.',
													A2(
														_elm_lang$core$Basics_ops['++'],
														_elm_community$elm_time$Time_Internal$padded3(
															_elm_community$elm_time$Time_ZonedDateTime$millisecond(dateTime)),
														_elm_community$elm_time$Time_ZonedDateTime$utcOffsetString(dateTime))))))))))))));
};
var _elm_community$elm_time$Time_ZonedDateTime$toDateTime = function (_p27) {
	var _p28 = _p27;
	return A3(
		_elm_lang$core$Basics$flip,
		_elm_community$elm_time$Time_DateTime$addMilliseconds,
		_p28._0.dateTime,
		_elm_community$elm_time$Time_ZonedDateTime$utcOffset(_p28));
};
var _elm_community$elm_time$Time_ZonedDateTime$zero = _elm_community$elm_time$Time_Internal$zero;
var _elm_community$elm_time$Time_ZonedDateTime$ZonedDateTime = function (a) {
	return {ctor: 'ZonedDateTime', _0: a};
};
var _elm_community$elm_time$Time_ZonedDateTime$zonedDateTime = F2(
	function (timeZone, dateTimeData) {
		return _elm_community$elm_time$Time_ZonedDateTime$ZonedDateTime(
			{
				timeZone: timeZone,
				dateTime: _elm_community$elm_time$Time_DateTime$dateTime(dateTimeData)
			});
	});
var _elm_community$elm_time$Time_ZonedDateTime$fromDateTime = F2(
	function (timeZone, dateTime) {
		var timestamp = _elm_community$elm_time$Time_DateTime$toTimestamp(dateTime);
		var offset = A2(_elm_community$elm_time$Time_TimeZone$offset, timestamp, timeZone);
		return _elm_community$elm_time$Time_ZonedDateTime$ZonedDateTime(
			{
				timeZone: timeZone,
				dateTime: A2(_elm_community$elm_time$Time_DateTime$addMilliseconds, 0 - offset, dateTime)
			});
	});
var _elm_community$elm_time$Time_ZonedDateTime$fromTimestamp = F2(
	function (timeZone, timestamp) {
		return A2(
			_elm_community$elm_time$Time_ZonedDateTime$fromDateTime,
			timeZone,
			_elm_community$elm_time$Time_DateTime$fromTimestamp(timestamp));
	});
var _elm_community$elm_time$Time_ZonedDateTime$asTimeZone = function (timeZone) {
	return function (_p29) {
		return A2(
			_elm_community$elm_time$Time_ZonedDateTime$fromDateTime,
			timeZone,
			_elm_community$elm_time$Time_ZonedDateTime$toDateTime(_p29));
	};
};
var _elm_community$elm_time$Time_ZonedDateTime$fromISO8601 = F2(
	function (timeZone, input) {
		return A2(
			_elm_lang$core$Result$map,
			_elm_community$elm_time$Time_ZonedDateTime$fromDateTime(timeZone),
			_elm_community$elm_time$Time_DateTime$fromISO8601(input));
	});
var _elm_community$elm_time$Time_ZonedDateTime$mapInner = F3(
	function (f, x, _p30) {
		var _p31 = _p30;
		return _elm_community$elm_time$Time_ZonedDateTime$ZonedDateTime(
			_elm_lang$core$Native_Utils.update(
				_p31._0,
				{
					dateTime: A2(f, x, _p31._0.dateTime)
				}));
	});
var _elm_community$elm_time$Time_ZonedDateTime$setDate = _elm_community$elm_time$Time_ZonedDateTime$mapInner(_elm_community$elm_time$Time_DateTime$setDate);
var _elm_community$elm_time$Time_ZonedDateTime$setYear = _elm_community$elm_time$Time_ZonedDateTime$mapInner(_elm_community$elm_time$Time_DateTime$setYear);
var _elm_community$elm_time$Time_ZonedDateTime$setMonth = _elm_community$elm_time$Time_ZonedDateTime$mapInner(_elm_community$elm_time$Time_DateTime$setMonth);
var _elm_community$elm_time$Time_ZonedDateTime$setDay = _elm_community$elm_time$Time_ZonedDateTime$mapInner(_elm_community$elm_time$Time_DateTime$setDay);
var _elm_community$elm_time$Time_ZonedDateTime$setHour = _elm_community$elm_time$Time_ZonedDateTime$mapInner(_elm_community$elm_time$Time_DateTime$setHour);
var _elm_community$elm_time$Time_ZonedDateTime$setMinute = _elm_community$elm_time$Time_ZonedDateTime$mapInner(_elm_community$elm_time$Time_DateTime$setMinute);
var _elm_community$elm_time$Time_ZonedDateTime$setSecond = _elm_community$elm_time$Time_ZonedDateTime$mapInner(_elm_community$elm_time$Time_DateTime$setSecond);
var _elm_community$elm_time$Time_ZonedDateTime$setMillisecond = _elm_community$elm_time$Time_ZonedDateTime$mapInner(_elm_community$elm_time$Time_DateTime$setMillisecond);
var _elm_community$elm_time$Time_ZonedDateTime$addYears = _elm_community$elm_time$Time_ZonedDateTime$mapInner(_elm_community$elm_time$Time_DateTime$addYears);
var _elm_community$elm_time$Time_ZonedDateTime$addMonths = _elm_community$elm_time$Time_ZonedDateTime$mapInner(_elm_community$elm_time$Time_DateTime$addMonths);
var _elm_community$elm_time$Time_ZonedDateTime$addDays = _elm_community$elm_time$Time_ZonedDateTime$mapInner(_elm_community$elm_time$Time_DateTime$addDays);
var _elm_community$elm_time$Time_ZonedDateTime$addHours = _elm_community$elm_time$Time_ZonedDateTime$mapInner(_elm_community$elm_time$Time_DateTime$addHours);
var _elm_community$elm_time$Time_ZonedDateTime$addMinutes = _elm_community$elm_time$Time_ZonedDateTime$mapInner(_elm_community$elm_time$Time_DateTime$addMinutes);
var _elm_community$elm_time$Time_ZonedDateTime$addSeconds = _elm_community$elm_time$Time_ZonedDateTime$mapInner(_elm_community$elm_time$Time_DateTime$addSeconds);
var _elm_community$elm_time$Time_ZonedDateTime$addMilliseconds = _elm_community$elm_time$Time_ZonedDateTime$mapInner(_elm_community$elm_time$Time_DateTime$addMilliseconds);

//import Native.List //

var _elm_lang$core$Native_Array = function() {

// A RRB-Tree has two distinct data types.
// Leaf -> "height"  is always 0
//         "table"   is an array of elements
// Node -> "height"  is always greater than 0
//         "table"   is an array of child nodes
//         "lengths" is an array of accumulated lengths of the child nodes

// M is the maximal table size. 32 seems fast. E is the allowed increase
// of search steps when concatting to find an index. Lower values will
// decrease balancing, but will increase search steps.
var M = 32;
var E = 2;

// An empty array.
var empty = {
	ctor: '_Array',
	height: 0,
	table: []
};


function get(i, array)
{
	if (i < 0 || i >= length(array))
	{
		throw new Error(
			'Index ' + i + ' is out of range. Check the length of ' +
			'your array first or use getMaybe or getWithDefault.');
	}
	return unsafeGet(i, array);
}


function unsafeGet(i, array)
{
	for (var x = array.height; x > 0; x--)
	{
		var slot = i >> (x * 5);
		while (array.lengths[slot] <= i)
		{
			slot++;
		}
		if (slot > 0)
		{
			i -= array.lengths[slot - 1];
		}
		array = array.table[slot];
	}
	return array.table[i];
}


// Sets the value at the index i. Only the nodes leading to i will get
// copied and updated.
function set(i, item, array)
{
	if (i < 0 || length(array) <= i)
	{
		return array;
	}
	return unsafeSet(i, item, array);
}


function unsafeSet(i, item, array)
{
	array = nodeCopy(array);

	if (array.height === 0)
	{
		array.table[i] = item;
	}
	else
	{
		var slot = getSlot(i, array);
		if (slot > 0)
		{
			i -= array.lengths[slot - 1];
		}
		array.table[slot] = unsafeSet(i, item, array.table[slot]);
	}
	return array;
}


function initialize(len, f)
{
	if (len <= 0)
	{
		return empty;
	}
	var h = Math.floor( Math.log(len) / Math.log(M) );
	return initialize_(f, h, 0, len);
}

function initialize_(f, h, from, to)
{
	if (h === 0)
	{
		var table = new Array((to - from) % (M + 1));
		for (var i = 0; i < table.length; i++)
		{
		  table[i] = f(from + i);
		}
		return {
			ctor: '_Array',
			height: 0,
			table: table
		};
	}

	var step = Math.pow(M, h);
	var table = new Array(Math.ceil((to - from) / step));
	var lengths = new Array(table.length);
	for (var i = 0; i < table.length; i++)
	{
		table[i] = initialize_(f, h - 1, from + (i * step), Math.min(from + ((i + 1) * step), to));
		lengths[i] = length(table[i]) + (i > 0 ? lengths[i-1] : 0);
	}
	return {
		ctor: '_Array',
		height: h,
		table: table,
		lengths: lengths
	};
}

function fromList(list)
{
	if (list.ctor === '[]')
	{
		return empty;
	}

	// Allocate M sized blocks (table) and write list elements to it.
	var table = new Array(M);
	var nodes = [];
	var i = 0;

	while (list.ctor !== '[]')
	{
		table[i] = list._0;
		list = list._1;
		i++;

		// table is full, so we can push a leaf containing it into the
		// next node.
		if (i === M)
		{
			var leaf = {
				ctor: '_Array',
				height: 0,
				table: table
			};
			fromListPush(leaf, nodes);
			table = new Array(M);
			i = 0;
		}
	}

	// Maybe there is something left on the table.
	if (i > 0)
	{
		var leaf = {
			ctor: '_Array',
			height: 0,
			table: table.splice(0, i)
		};
		fromListPush(leaf, nodes);
	}

	// Go through all of the nodes and eventually push them into higher nodes.
	for (var h = 0; h < nodes.length - 1; h++)
	{
		if (nodes[h].table.length > 0)
		{
			fromListPush(nodes[h], nodes);
		}
	}

	var head = nodes[nodes.length - 1];
	if (head.height > 0 && head.table.length === 1)
	{
		return head.table[0];
	}
	else
	{
		return head;
	}
}

// Push a node into a higher node as a child.
function fromListPush(toPush, nodes)
{
	var h = toPush.height;

	// Maybe the node on this height does not exist.
	if (nodes.length === h)
	{
		var node = {
			ctor: '_Array',
			height: h + 1,
			table: [],
			lengths: []
		};
		nodes.push(node);
	}

	nodes[h].table.push(toPush);
	var len = length(toPush);
	if (nodes[h].lengths.length > 0)
	{
		len += nodes[h].lengths[nodes[h].lengths.length - 1];
	}
	nodes[h].lengths.push(len);

	if (nodes[h].table.length === M)
	{
		fromListPush(nodes[h], nodes);
		nodes[h] = {
			ctor: '_Array',
			height: h + 1,
			table: [],
			lengths: []
		};
	}
}

// Pushes an item via push_ to the bottom right of a tree.
function push(item, a)
{
	var pushed = push_(item, a);
	if (pushed !== null)
	{
		return pushed;
	}

	var newTree = create(item, a.height);
	return siblise(a, newTree);
}

// Recursively tries to push an item to the bottom-right most
// tree possible. If there is no space left for the item,
// null will be returned.
function push_(item, a)
{
	// Handle resursion stop at leaf level.
	if (a.height === 0)
	{
		if (a.table.length < M)
		{
			var newA = {
				ctor: '_Array',
				height: 0,
				table: a.table.slice()
			};
			newA.table.push(item);
			return newA;
		}
		else
		{
		  return null;
		}
	}

	// Recursively push
	var pushed = push_(item, botRight(a));

	// There was space in the bottom right tree, so the slot will
	// be updated.
	if (pushed !== null)
	{
		var newA = nodeCopy(a);
		newA.table[newA.table.length - 1] = pushed;
		newA.lengths[newA.lengths.length - 1]++;
		return newA;
	}

	// When there was no space left, check if there is space left
	// for a new slot with a tree which contains only the item
	// at the bottom.
	if (a.table.length < M)
	{
		var newSlot = create(item, a.height - 1);
		var newA = nodeCopy(a);
		newA.table.push(newSlot);
		newA.lengths.push(newA.lengths[newA.lengths.length - 1] + length(newSlot));
		return newA;
	}
	else
	{
		return null;
	}
}

// Converts an array into a list of elements.
function toList(a)
{
	return toList_(_elm_lang$core$Native_List.Nil, a);
}

function toList_(list, a)
{
	for (var i = a.table.length - 1; i >= 0; i--)
	{
		list =
			a.height === 0
				? _elm_lang$core$Native_List.Cons(a.table[i], list)
				: toList_(list, a.table[i]);
	}
	return list;
}

// Maps a function over the elements of an array.
function map(f, a)
{
	var newA = {
		ctor: '_Array',
		height: a.height,
		table: new Array(a.table.length)
	};
	if (a.height > 0)
	{
		newA.lengths = a.lengths;
	}
	for (var i = 0; i < a.table.length; i++)
	{
		newA.table[i] =
			a.height === 0
				? f(a.table[i])
				: map(f, a.table[i]);
	}
	return newA;
}

// Maps a function over the elements with their index as first argument.
function indexedMap(f, a)
{
	return indexedMap_(f, a, 0);
}

function indexedMap_(f, a, from)
{
	var newA = {
		ctor: '_Array',
		height: a.height,
		table: new Array(a.table.length)
	};
	if (a.height > 0)
	{
		newA.lengths = a.lengths;
	}
	for (var i = 0; i < a.table.length; i++)
	{
		newA.table[i] =
			a.height === 0
				? A2(f, from + i, a.table[i])
				: indexedMap_(f, a.table[i], i == 0 ? from : from + a.lengths[i - 1]);
	}
	return newA;
}

function foldl(f, b, a)
{
	if (a.height === 0)
	{
		for (var i = 0; i < a.table.length; i++)
		{
			b = A2(f, a.table[i], b);
		}
	}
	else
	{
		for (var i = 0; i < a.table.length; i++)
		{
			b = foldl(f, b, a.table[i]);
		}
	}
	return b;
}

function foldr(f, b, a)
{
	if (a.height === 0)
	{
		for (var i = a.table.length; i--; )
		{
			b = A2(f, a.table[i], b);
		}
	}
	else
	{
		for (var i = a.table.length; i--; )
		{
			b = foldr(f, b, a.table[i]);
		}
	}
	return b;
}

// TODO: currently, it slices the right, then the left. This can be
// optimized.
function slice(from, to, a)
{
	if (from < 0)
	{
		from += length(a);
	}
	if (to < 0)
	{
		to += length(a);
	}
	return sliceLeft(from, sliceRight(to, a));
}

function sliceRight(to, a)
{
	if (to === length(a))
	{
		return a;
	}

	// Handle leaf level.
	if (a.height === 0)
	{
		var newA = { ctor:'_Array', height:0 };
		newA.table = a.table.slice(0, to);
		return newA;
	}

	// Slice the right recursively.
	var right = getSlot(to, a);
	var sliced = sliceRight(to - (right > 0 ? a.lengths[right - 1] : 0), a.table[right]);

	// Maybe the a node is not even needed, as sliced contains the whole slice.
	if (right === 0)
	{
		return sliced;
	}

	// Create new node.
	var newA = {
		ctor: '_Array',
		height: a.height,
		table: a.table.slice(0, right),
		lengths: a.lengths.slice(0, right)
	};
	if (sliced.table.length > 0)
	{
		newA.table[right] = sliced;
		newA.lengths[right] = length(sliced) + (right > 0 ? newA.lengths[right - 1] : 0);
	}
	return newA;
}

function sliceLeft(from, a)
{
	if (from === 0)
	{
		return a;
	}

	// Handle leaf level.
	if (a.height === 0)
	{
		var newA = { ctor:'_Array', height:0 };
		newA.table = a.table.slice(from, a.table.length + 1);
		return newA;
	}

	// Slice the left recursively.
	var left = getSlot(from, a);
	var sliced = sliceLeft(from - (left > 0 ? a.lengths[left - 1] : 0), a.table[left]);

	// Maybe the a node is not even needed, as sliced contains the whole slice.
	if (left === a.table.length - 1)
	{
		return sliced;
	}

	// Create new node.
	var newA = {
		ctor: '_Array',
		height: a.height,
		table: a.table.slice(left, a.table.length + 1),
		lengths: new Array(a.table.length - left)
	};
	newA.table[0] = sliced;
	var len = 0;
	for (var i = 0; i < newA.table.length; i++)
	{
		len += length(newA.table[i]);
		newA.lengths[i] = len;
	}

	return newA;
}

// Appends two trees.
function append(a,b)
{
	if (a.table.length === 0)
	{
		return b;
	}
	if (b.table.length === 0)
	{
		return a;
	}

	var c = append_(a, b);

	// Check if both nodes can be crunshed together.
	if (c[0].table.length + c[1].table.length <= M)
	{
		if (c[0].table.length === 0)
		{
			return c[1];
		}
		if (c[1].table.length === 0)
		{
			return c[0];
		}

		// Adjust .table and .lengths
		c[0].table = c[0].table.concat(c[1].table);
		if (c[0].height > 0)
		{
			var len = length(c[0]);
			for (var i = 0; i < c[1].lengths.length; i++)
			{
				c[1].lengths[i] += len;
			}
			c[0].lengths = c[0].lengths.concat(c[1].lengths);
		}

		return c[0];
	}

	if (c[0].height > 0)
	{
		var toRemove = calcToRemove(a, b);
		if (toRemove > E)
		{
			c = shuffle(c[0], c[1], toRemove);
		}
	}

	return siblise(c[0], c[1]);
}

// Returns an array of two nodes; right and left. One node _may_ be empty.
function append_(a, b)
{
	if (a.height === 0 && b.height === 0)
	{
		return [a, b];
	}

	if (a.height !== 1 || b.height !== 1)
	{
		if (a.height === b.height)
		{
			a = nodeCopy(a);
			b = nodeCopy(b);
			var appended = append_(botRight(a), botLeft(b));

			insertRight(a, appended[1]);
			insertLeft(b, appended[0]);
		}
		else if (a.height > b.height)
		{
			a = nodeCopy(a);
			var appended = append_(botRight(a), b);

			insertRight(a, appended[0]);
			b = parentise(appended[1], appended[1].height + 1);
		}
		else
		{
			b = nodeCopy(b);
			var appended = append_(a, botLeft(b));

			var left = appended[0].table.length === 0 ? 0 : 1;
			var right = left === 0 ? 1 : 0;
			insertLeft(b, appended[left]);
			a = parentise(appended[right], appended[right].height + 1);
		}
	}

	// Check if balancing is needed and return based on that.
	if (a.table.length === 0 || b.table.length === 0)
	{
		return [a, b];
	}

	var toRemove = calcToRemove(a, b);
	if (toRemove <= E)
	{
		return [a, b];
	}
	return shuffle(a, b, toRemove);
}

// Helperfunctions for append_. Replaces a child node at the side of the parent.
function insertRight(parent, node)
{
	var index = parent.table.length - 1;
	parent.table[index] = node;
	parent.lengths[index] = length(node);
	parent.lengths[index] += index > 0 ? parent.lengths[index - 1] : 0;
}

function insertLeft(parent, node)
{
	if (node.table.length > 0)
	{
		parent.table[0] = node;
		parent.lengths[0] = length(node);

		var len = length(parent.table[0]);
		for (var i = 1; i < parent.lengths.length; i++)
		{
			len += length(parent.table[i]);
			parent.lengths[i] = len;
		}
	}
	else
	{
		parent.table.shift();
		for (var i = 1; i < parent.lengths.length; i++)
		{
			parent.lengths[i] = parent.lengths[i] - parent.lengths[0];
		}
		parent.lengths.shift();
	}
}

// Returns the extra search steps for E. Refer to the paper.
function calcToRemove(a, b)
{
	var subLengths = 0;
	for (var i = 0; i < a.table.length; i++)
	{
		subLengths += a.table[i].table.length;
	}
	for (var i = 0; i < b.table.length; i++)
	{
		subLengths += b.table[i].table.length;
	}

	var toRemove = a.table.length + b.table.length;
	return toRemove - (Math.floor((subLengths - 1) / M) + 1);
}

// get2, set2 and saveSlot are helpers for accessing elements over two arrays.
function get2(a, b, index)
{
	return index < a.length
		? a[index]
		: b[index - a.length];
}

function set2(a, b, index, value)
{
	if (index < a.length)
	{
		a[index] = value;
	}
	else
	{
		b[index - a.length] = value;
	}
}

function saveSlot(a, b, index, slot)
{
	set2(a.table, b.table, index, slot);

	var l = (index === 0 || index === a.lengths.length)
		? 0
		: get2(a.lengths, a.lengths, index - 1);

	set2(a.lengths, b.lengths, index, l + length(slot));
}

// Creates a node or leaf with a given length at their arrays for perfomance.
// Is only used by shuffle.
function createNode(h, length)
{
	if (length < 0)
	{
		length = 0;
	}
	var a = {
		ctor: '_Array',
		height: h,
		table: new Array(length)
	};
	if (h > 0)
	{
		a.lengths = new Array(length);
	}
	return a;
}

// Returns an array of two balanced nodes.
function shuffle(a, b, toRemove)
{
	var newA = createNode(a.height, Math.min(M, a.table.length + b.table.length - toRemove));
	var newB = createNode(a.height, newA.table.length - (a.table.length + b.table.length - toRemove));

	// Skip the slots with size M. More precise: copy the slot references
	// to the new node
	var read = 0;
	while (get2(a.table, b.table, read).table.length % M === 0)
	{
		set2(newA.table, newB.table, read, get2(a.table, b.table, read));
		set2(newA.lengths, newB.lengths, read, get2(a.lengths, b.lengths, read));
		read++;
	}

	// Pulling items from left to right, caching in a slot before writing
	// it into the new nodes.
	var write = read;
	var slot = new createNode(a.height - 1, 0);
	var from = 0;

	// If the current slot is still containing data, then there will be at
	// least one more write, so we do not break this loop yet.
	while (read - write - (slot.table.length > 0 ? 1 : 0) < toRemove)
	{
		// Find out the max possible items for copying.
		var source = get2(a.table, b.table, read);
		var to = Math.min(M - slot.table.length, source.table.length);

		// Copy and adjust size table.
		slot.table = slot.table.concat(source.table.slice(from, to));
		if (slot.height > 0)
		{
			var len = slot.lengths.length;
			for (var i = len; i < len + to - from; i++)
			{
				slot.lengths[i] = length(slot.table[i]);
				slot.lengths[i] += (i > 0 ? slot.lengths[i - 1] : 0);
			}
		}

		from += to;

		// Only proceed to next slots[i] if the current one was
		// fully copied.
		if (source.table.length <= to)
		{
			read++; from = 0;
		}

		// Only create a new slot if the current one is filled up.
		if (slot.table.length === M)
		{
			saveSlot(newA, newB, write, slot);
			slot = createNode(a.height - 1, 0);
			write++;
		}
	}

	// Cleanup after the loop. Copy the last slot into the new nodes.
	if (slot.table.length > 0)
	{
		saveSlot(newA, newB, write, slot);
		write++;
	}

	// Shift the untouched slots to the left
	while (read < a.table.length + b.table.length )
	{
		saveSlot(newA, newB, write, get2(a.table, b.table, read));
		read++;
		write++;
	}

	return [newA, newB];
}

// Navigation functions
function botRight(a)
{
	return a.table[a.table.length - 1];
}
function botLeft(a)
{
	return a.table[0];
}

// Copies a node for updating. Note that you should not use this if
// only updating only one of "table" or "lengths" for performance reasons.
function nodeCopy(a)
{
	var newA = {
		ctor: '_Array',
		height: a.height,
		table: a.table.slice()
	};
	if (a.height > 0)
	{
		newA.lengths = a.lengths.slice();
	}
	return newA;
}

// Returns how many items are in the tree.
function length(array)
{
	if (array.height === 0)
	{
		return array.table.length;
	}
	else
	{
		return array.lengths[array.lengths.length - 1];
	}
}

// Calculates in which slot of "table" the item probably is, then
// find the exact slot via forward searching in  "lengths". Returns the index.
function getSlot(i, a)
{
	var slot = i >> (5 * a.height);
	while (a.lengths[slot] <= i)
	{
		slot++;
	}
	return slot;
}

// Recursively creates a tree with a given height containing
// only the given item.
function create(item, h)
{
	if (h === 0)
	{
		return {
			ctor: '_Array',
			height: 0,
			table: [item]
		};
	}
	return {
		ctor: '_Array',
		height: h,
		table: [create(item, h - 1)],
		lengths: [1]
	};
}

// Recursively creates a tree that contains the given tree.
function parentise(tree, h)
{
	if (h === tree.height)
	{
		return tree;
	}

	return {
		ctor: '_Array',
		height: h,
		table: [parentise(tree, h - 1)],
		lengths: [length(tree)]
	};
}

// Emphasizes blood brotherhood beneath two trees.
function siblise(a, b)
{
	return {
		ctor: '_Array',
		height: a.height + 1,
		table: [a, b],
		lengths: [length(a), length(a) + length(b)]
	};
}

function toJSArray(a)
{
	var jsArray = new Array(length(a));
	toJSArray_(jsArray, 0, a);
	return jsArray;
}

function toJSArray_(jsArray, i, a)
{
	for (var t = 0; t < a.table.length; t++)
	{
		if (a.height === 0)
		{
			jsArray[i + t] = a.table[t];
		}
		else
		{
			var inc = t === 0 ? 0 : a.lengths[t - 1];
			toJSArray_(jsArray, i + inc, a.table[t]);
		}
	}
}

function fromJSArray(jsArray)
{
	if (jsArray.length === 0)
	{
		return empty;
	}
	var h = Math.floor(Math.log(jsArray.length) / Math.log(M));
	return fromJSArray_(jsArray, h, 0, jsArray.length);
}

function fromJSArray_(jsArray, h, from, to)
{
	if (h === 0)
	{
		return {
			ctor: '_Array',
			height: 0,
			table: jsArray.slice(from, to)
		};
	}

	var step = Math.pow(M, h);
	var table = new Array(Math.ceil((to - from) / step));
	var lengths = new Array(table.length);
	for (var i = 0; i < table.length; i++)
	{
		table[i] = fromJSArray_(jsArray, h - 1, from + (i * step), Math.min(from + ((i + 1) * step), to));
		lengths[i] = length(table[i]) + (i > 0 ? lengths[i - 1] : 0);
	}
	return {
		ctor: '_Array',
		height: h,
		table: table,
		lengths: lengths
	};
}

return {
	empty: empty,
	fromList: fromList,
	toList: toList,
	initialize: F2(initialize),
	append: F2(append),
	push: F2(push),
	slice: F3(slice),
	get: F2(get),
	set: F3(set),
	map: F2(map),
	indexedMap: F2(indexedMap),
	foldl: F3(foldl),
	foldr: F3(foldr),
	length: length,

	toJSArray: toJSArray,
	fromJSArray: fromJSArray
};

}();
var _elm_lang$core$Array$append = _elm_lang$core$Native_Array.append;
var _elm_lang$core$Array$length = _elm_lang$core$Native_Array.length;
var _elm_lang$core$Array$isEmpty = function (array) {
	return _elm_lang$core$Native_Utils.eq(
		_elm_lang$core$Array$length(array),
		0);
};
var _elm_lang$core$Array$slice = _elm_lang$core$Native_Array.slice;
var _elm_lang$core$Array$set = _elm_lang$core$Native_Array.set;
var _elm_lang$core$Array$get = F2(
	function (i, array) {
		return ((_elm_lang$core$Native_Utils.cmp(0, i) < 1) && (_elm_lang$core$Native_Utils.cmp(
			i,
			_elm_lang$core$Native_Array.length(array)) < 0)) ? _elm_lang$core$Maybe$Just(
			A2(_elm_lang$core$Native_Array.get, i, array)) : _elm_lang$core$Maybe$Nothing;
	});
var _elm_lang$core$Array$push = _elm_lang$core$Native_Array.push;
var _elm_lang$core$Array$empty = _elm_lang$core$Native_Array.empty;
var _elm_lang$core$Array$filter = F2(
	function (isOkay, arr) {
		var update = F2(
			function (x, xs) {
				return isOkay(x) ? A2(_elm_lang$core$Native_Array.push, x, xs) : xs;
			});
		return A3(_elm_lang$core$Native_Array.foldl, update, _elm_lang$core$Native_Array.empty, arr);
	});
var _elm_lang$core$Array$foldr = _elm_lang$core$Native_Array.foldr;
var _elm_lang$core$Array$foldl = _elm_lang$core$Native_Array.foldl;
var _elm_lang$core$Array$indexedMap = _elm_lang$core$Native_Array.indexedMap;
var _elm_lang$core$Array$map = _elm_lang$core$Native_Array.map;
var _elm_lang$core$Array$toIndexedList = function (array) {
	return A3(
		_elm_lang$core$List$map2,
		F2(
			function (v0, v1) {
				return {ctor: '_Tuple2', _0: v0, _1: v1};
			}),
		A2(
			_elm_lang$core$List$range,
			0,
			_elm_lang$core$Native_Array.length(array) - 1),
		_elm_lang$core$Native_Array.toList(array));
};
var _elm_lang$core$Array$toList = _elm_lang$core$Native_Array.toList;
var _elm_lang$core$Array$fromList = _elm_lang$core$Native_Array.fromList;
var _elm_lang$core$Array$initialize = _elm_lang$core$Native_Array.initialize;
var _elm_lang$core$Array$repeat = F2(
	function (n, e) {
		return A2(
			_elm_lang$core$Array$initialize,
			n,
			_elm_lang$core$Basics$always(e));
	});
var _elm_lang$core$Array$Array = {ctor: 'Array'};

//import Maybe, Native.Array, Native.List, Native.Utils, Result //

var _elm_lang$core$Native_Json = function() {


// CORE DECODERS

function succeed(msg)
{
	return {
		ctor: '<decoder>',
		tag: 'succeed',
		msg: msg
	};
}

function fail(msg)
{
	return {
		ctor: '<decoder>',
		tag: 'fail',
		msg: msg
	};
}

function decodePrimitive(tag)
{
	return {
		ctor: '<decoder>',
		tag: tag
	};
}

function decodeContainer(tag, decoder)
{
	return {
		ctor: '<decoder>',
		tag: tag,
		decoder: decoder
	};
}

function decodeNull(value)
{
	return {
		ctor: '<decoder>',
		tag: 'null',
		value: value
	};
}

function decodeField(field, decoder)
{
	return {
		ctor: '<decoder>',
		tag: 'field',
		field: field,
		decoder: decoder
	};
}

function decodeIndex(index, decoder)
{
	return {
		ctor: '<decoder>',
		tag: 'index',
		index: index,
		decoder: decoder
	};
}

function decodeKeyValuePairs(decoder)
{
	return {
		ctor: '<decoder>',
		tag: 'key-value',
		decoder: decoder
	};
}

function mapMany(f, decoders)
{
	return {
		ctor: '<decoder>',
		tag: 'map-many',
		func: f,
		decoders: decoders
	};
}

function andThen(callback, decoder)
{
	return {
		ctor: '<decoder>',
		tag: 'andThen',
		decoder: decoder,
		callback: callback
	};
}

function oneOf(decoders)
{
	return {
		ctor: '<decoder>',
		tag: 'oneOf',
		decoders: decoders
	};
}


// DECODING OBJECTS

function map1(f, d1)
{
	return mapMany(f, [d1]);
}

function map2(f, d1, d2)
{
	return mapMany(f, [d1, d2]);
}

function map3(f, d1, d2, d3)
{
	return mapMany(f, [d1, d2, d3]);
}

function map4(f, d1, d2, d3, d4)
{
	return mapMany(f, [d1, d2, d3, d4]);
}

function map5(f, d1, d2, d3, d4, d5)
{
	return mapMany(f, [d1, d2, d3, d4, d5]);
}

function map6(f, d1, d2, d3, d4, d5, d6)
{
	return mapMany(f, [d1, d2, d3, d4, d5, d6]);
}

function map7(f, d1, d2, d3, d4, d5, d6, d7)
{
	return mapMany(f, [d1, d2, d3, d4, d5, d6, d7]);
}

function map8(f, d1, d2, d3, d4, d5, d6, d7, d8)
{
	return mapMany(f, [d1, d2, d3, d4, d5, d6, d7, d8]);
}


// DECODE HELPERS

function ok(value)
{
	return { tag: 'ok', value: value };
}

function badPrimitive(type, value)
{
	return { tag: 'primitive', type: type, value: value };
}

function badIndex(index, nestedProblems)
{
	return { tag: 'index', index: index, rest: nestedProblems };
}

function badField(field, nestedProblems)
{
	return { tag: 'field', field: field, rest: nestedProblems };
}

function badIndex(index, nestedProblems)
{
	return { tag: 'index', index: index, rest: nestedProblems };
}

function badOneOf(problems)
{
	return { tag: 'oneOf', problems: problems };
}

function bad(msg)
{
	return { tag: 'fail', msg: msg };
}

function badToString(problem)
{
	var context = '_';
	while (problem)
	{
		switch (problem.tag)
		{
			case 'primitive':
				return 'Expecting ' + problem.type
					+ (context === '_' ? '' : ' at ' + context)
					+ ' but instead got: ' + jsToString(problem.value);

			case 'index':
				context += '[' + problem.index + ']';
				problem = problem.rest;
				break;

			case 'field':
				context += '.' + problem.field;
				problem = problem.rest;
				break;

			case 'oneOf':
				var problems = problem.problems;
				for (var i = 0; i < problems.length; i++)
				{
					problems[i] = badToString(problems[i]);
				}
				return 'I ran into the following problems'
					+ (context === '_' ? '' : ' at ' + context)
					+ ':\n\n' + problems.join('\n');

			case 'fail':
				return 'I ran into a `fail` decoder'
					+ (context === '_' ? '' : ' at ' + context)
					+ ': ' + problem.msg;
		}
	}
}

function jsToString(value)
{
	return value === undefined
		? 'undefined'
		: JSON.stringify(value);
}


// DECODE

function runOnString(decoder, string)
{
	var json;
	try
	{
		json = JSON.parse(string);
	}
	catch (e)
	{
		return _elm_lang$core$Result$Err('Given an invalid JSON: ' + e.message);
	}
	return run(decoder, json);
}

function run(decoder, value)
{
	var result = runHelp(decoder, value);
	return (result.tag === 'ok')
		? _elm_lang$core$Result$Ok(result.value)
		: _elm_lang$core$Result$Err(badToString(result));
}

function runHelp(decoder, value)
{
	switch (decoder.tag)
	{
		case 'bool':
			return (typeof value === 'boolean')
				? ok(value)
				: badPrimitive('a Bool', value);

		case 'int':
			if (typeof value !== 'number') {
				return badPrimitive('an Int', value);
			}

			if (-2147483647 < value && value < 2147483647 && (value | 0) === value) {
				return ok(value);
			}

			if (isFinite(value) && !(value % 1)) {
				return ok(value);
			}

			return badPrimitive('an Int', value);

		case 'float':
			return (typeof value === 'number')
				? ok(value)
				: badPrimitive('a Float', value);

		case 'string':
			return (typeof value === 'string')
				? ok(value)
				: (value instanceof String)
					? ok(value + '')
					: badPrimitive('a String', value);

		case 'null':
			return (value === null)
				? ok(decoder.value)
				: badPrimitive('null', value);

		case 'value':
			return ok(value);

		case 'list':
			if (!(value instanceof Array))
			{
				return badPrimitive('a List', value);
			}

			var list = _elm_lang$core$Native_List.Nil;
			for (var i = value.length; i--; )
			{
				var result = runHelp(decoder.decoder, value[i]);
				if (result.tag !== 'ok')
				{
					return badIndex(i, result)
				}
				list = _elm_lang$core$Native_List.Cons(result.value, list);
			}
			return ok(list);

		case 'array':
			if (!(value instanceof Array))
			{
				return badPrimitive('an Array', value);
			}

			var len = value.length;
			var array = new Array(len);
			for (var i = len; i--; )
			{
				var result = runHelp(decoder.decoder, value[i]);
				if (result.tag !== 'ok')
				{
					return badIndex(i, result);
				}
				array[i] = result.value;
			}
			return ok(_elm_lang$core$Native_Array.fromJSArray(array));

		case 'maybe':
			var result = runHelp(decoder.decoder, value);
			return (result.tag === 'ok')
				? ok(_elm_lang$core$Maybe$Just(result.value))
				: ok(_elm_lang$core$Maybe$Nothing);

		case 'field':
			var field = decoder.field;
			if (typeof value !== 'object' || value === null || !(field in value))
			{
				return badPrimitive('an object with a field named `' + field + '`', value);
			}

			var result = runHelp(decoder.decoder, value[field]);
			return (result.tag === 'ok') ? result : badField(field, result);

		case 'index':
			var index = decoder.index;
			if (!(value instanceof Array))
			{
				return badPrimitive('an array', value);
			}
			if (index >= value.length)
			{
				return badPrimitive('a longer array. Need index ' + index + ' but there are only ' + value.length + ' entries', value);
			}

			var result = runHelp(decoder.decoder, value[index]);
			return (result.tag === 'ok') ? result : badIndex(index, result);

		case 'key-value':
			if (typeof value !== 'object' || value === null || value instanceof Array)
			{
				return badPrimitive('an object', value);
			}

			var keyValuePairs = _elm_lang$core$Native_List.Nil;
			for (var key in value)
			{
				var result = runHelp(decoder.decoder, value[key]);
				if (result.tag !== 'ok')
				{
					return badField(key, result);
				}
				var pair = _elm_lang$core$Native_Utils.Tuple2(key, result.value);
				keyValuePairs = _elm_lang$core$Native_List.Cons(pair, keyValuePairs);
			}
			return ok(keyValuePairs);

		case 'map-many':
			var answer = decoder.func;
			var decoders = decoder.decoders;
			for (var i = 0; i < decoders.length; i++)
			{
				var result = runHelp(decoders[i], value);
				if (result.tag !== 'ok')
				{
					return result;
				}
				answer = answer(result.value);
			}
			return ok(answer);

		case 'andThen':
			var result = runHelp(decoder.decoder, value);
			return (result.tag !== 'ok')
				? result
				: runHelp(decoder.callback(result.value), value);

		case 'oneOf':
			var errors = [];
			var temp = decoder.decoders;
			while (temp.ctor !== '[]')
			{
				var result = runHelp(temp._0, value);

				if (result.tag === 'ok')
				{
					return result;
				}

				errors.push(result);

				temp = temp._1;
			}
			return badOneOf(errors);

		case 'fail':
			return bad(decoder.msg);

		case 'succeed':
			return ok(decoder.msg);
	}
}


// EQUALITY

function equality(a, b)
{
	if (a === b)
	{
		return true;
	}

	if (a.tag !== b.tag)
	{
		return false;
	}

	switch (a.tag)
	{
		case 'succeed':
		case 'fail':
			return a.msg === b.msg;

		case 'bool':
		case 'int':
		case 'float':
		case 'string':
		case 'value':
			return true;

		case 'null':
			return a.value === b.value;

		case 'list':
		case 'array':
		case 'maybe':
		case 'key-value':
			return equality(a.decoder, b.decoder);

		case 'field':
			return a.field === b.field && equality(a.decoder, b.decoder);

		case 'index':
			return a.index === b.index && equality(a.decoder, b.decoder);

		case 'map-many':
			if (a.func !== b.func)
			{
				return false;
			}
			return listEquality(a.decoders, b.decoders);

		case 'andThen':
			return a.callback === b.callback && equality(a.decoder, b.decoder);

		case 'oneOf':
			return listEquality(a.decoders, b.decoders);
	}
}

function listEquality(aDecoders, bDecoders)
{
	var len = aDecoders.length;
	if (len !== bDecoders.length)
	{
		return false;
	}
	for (var i = 0; i < len; i++)
	{
		if (!equality(aDecoders[i], bDecoders[i]))
		{
			return false;
		}
	}
	return true;
}


// ENCODE

function encode(indentLevel, value)
{
	return JSON.stringify(value, null, indentLevel);
}

function identity(value)
{
	return value;
}

function encodeObject(keyValuePairs)
{
	var obj = {};
	while (keyValuePairs.ctor !== '[]')
	{
		var pair = keyValuePairs._0;
		obj[pair._0] = pair._1;
		keyValuePairs = keyValuePairs._1;
	}
	return obj;
}

return {
	encode: F2(encode),
	runOnString: F2(runOnString),
	run: F2(run),

	decodeNull: decodeNull,
	decodePrimitive: decodePrimitive,
	decodeContainer: F2(decodeContainer),

	decodeField: F2(decodeField),
	decodeIndex: F2(decodeIndex),

	map1: F2(map1),
	map2: F3(map2),
	map3: F4(map3),
	map4: F5(map4),
	map5: F6(map5),
	map6: F7(map6),
	map7: F8(map7),
	map8: F9(map8),
	decodeKeyValuePairs: decodeKeyValuePairs,

	andThen: F2(andThen),
	fail: fail,
	succeed: succeed,
	oneOf: oneOf,

	identity: identity,
	encodeNull: null,
	encodeArray: _elm_lang$core$Native_Array.toJSArray,
	encodeList: _elm_lang$core$Native_List.toArray,
	encodeObject: encodeObject,

	equality: equality
};

}();

var _elm_lang$core$Json_Encode$list = _elm_lang$core$Native_Json.encodeList;
var _elm_lang$core$Json_Encode$array = _elm_lang$core$Native_Json.encodeArray;
var _elm_lang$core$Json_Encode$object = _elm_lang$core$Native_Json.encodeObject;
var _elm_lang$core$Json_Encode$null = _elm_lang$core$Native_Json.encodeNull;
var _elm_lang$core$Json_Encode$bool = _elm_lang$core$Native_Json.identity;
var _elm_lang$core$Json_Encode$float = _elm_lang$core$Native_Json.identity;
var _elm_lang$core$Json_Encode$int = _elm_lang$core$Native_Json.identity;
var _elm_lang$core$Json_Encode$string = _elm_lang$core$Native_Json.identity;
var _elm_lang$core$Json_Encode$encode = _elm_lang$core$Native_Json.encode;
var _elm_lang$core$Json_Encode$Value = {ctor: 'Value'};

var _elm_lang$core$Json_Decode$null = _elm_lang$core$Native_Json.decodeNull;
var _elm_lang$core$Json_Decode$value = _elm_lang$core$Native_Json.decodePrimitive('value');
var _elm_lang$core$Json_Decode$andThen = _elm_lang$core$Native_Json.andThen;
var _elm_lang$core$Json_Decode$fail = _elm_lang$core$Native_Json.fail;
var _elm_lang$core$Json_Decode$succeed = _elm_lang$core$Native_Json.succeed;
var _elm_lang$core$Json_Decode$lazy = function (thunk) {
	return A2(
		_elm_lang$core$Json_Decode$andThen,
		thunk,
		_elm_lang$core$Json_Decode$succeed(
			{ctor: '_Tuple0'}));
};
var _elm_lang$core$Json_Decode$decodeValue = _elm_lang$core$Native_Json.run;
var _elm_lang$core$Json_Decode$decodeString = _elm_lang$core$Native_Json.runOnString;
var _elm_lang$core$Json_Decode$map8 = _elm_lang$core$Native_Json.map8;
var _elm_lang$core$Json_Decode$map7 = _elm_lang$core$Native_Json.map7;
var _elm_lang$core$Json_Decode$map6 = _elm_lang$core$Native_Json.map6;
var _elm_lang$core$Json_Decode$map5 = _elm_lang$core$Native_Json.map5;
var _elm_lang$core$Json_Decode$map4 = _elm_lang$core$Native_Json.map4;
var _elm_lang$core$Json_Decode$map3 = _elm_lang$core$Native_Json.map3;
var _elm_lang$core$Json_Decode$map2 = _elm_lang$core$Native_Json.map2;
var _elm_lang$core$Json_Decode$map = _elm_lang$core$Native_Json.map1;
var _elm_lang$core$Json_Decode$oneOf = _elm_lang$core$Native_Json.oneOf;
var _elm_lang$core$Json_Decode$maybe = function (decoder) {
	return A2(_elm_lang$core$Native_Json.decodeContainer, 'maybe', decoder);
};
var _elm_lang$core$Json_Decode$index = _elm_lang$core$Native_Json.decodeIndex;
var _elm_lang$core$Json_Decode$field = _elm_lang$core$Native_Json.decodeField;
var _elm_lang$core$Json_Decode$at = F2(
	function (fields, decoder) {
		return A3(_elm_lang$core$List$foldr, _elm_lang$core$Json_Decode$field, decoder, fields);
	});
var _elm_lang$core$Json_Decode$keyValuePairs = _elm_lang$core$Native_Json.decodeKeyValuePairs;
var _elm_lang$core$Json_Decode$dict = function (decoder) {
	return A2(
		_elm_lang$core$Json_Decode$map,
		_elm_lang$core$Dict$fromList,
		_elm_lang$core$Json_Decode$keyValuePairs(decoder));
};
var _elm_lang$core$Json_Decode$array = function (decoder) {
	return A2(_elm_lang$core$Native_Json.decodeContainer, 'array', decoder);
};
var _elm_lang$core$Json_Decode$list = function (decoder) {
	return A2(_elm_lang$core$Native_Json.decodeContainer, 'list', decoder);
};
var _elm_lang$core$Json_Decode$nullable = function (decoder) {
	return _elm_lang$core$Json_Decode$oneOf(
		{
			ctor: '::',
			_0: _elm_lang$core$Json_Decode$null(_elm_lang$core$Maybe$Nothing),
			_1: {
				ctor: '::',
				_0: A2(_elm_lang$core$Json_Decode$map, _elm_lang$core$Maybe$Just, decoder),
				_1: {ctor: '[]'}
			}
		});
};
var _elm_lang$core$Json_Decode$float = _elm_lang$core$Native_Json.decodePrimitive('float');
var _elm_lang$core$Json_Decode$int = _elm_lang$core$Native_Json.decodePrimitive('int');
var _elm_lang$core$Json_Decode$bool = _elm_lang$core$Native_Json.decodePrimitive('bool');
var _elm_lang$core$Json_Decode$string = _elm_lang$core$Native_Json.decodePrimitive('string');
var _elm_lang$core$Json_Decode$Decoder = {ctor: 'Decoder'};

var _elm_lang$virtual_dom$VirtualDom_Debug$wrap;
var _elm_lang$virtual_dom$VirtualDom_Debug$wrapWithFlags;

var _elm_lang$virtual_dom$Native_VirtualDom = function() {

var STYLE_KEY = 'STYLE';
var EVENT_KEY = 'EVENT';
var ATTR_KEY = 'ATTR';
var ATTR_NS_KEY = 'ATTR_NS';

var localDoc = typeof document !== 'undefined' ? document : {};


////////////  VIRTUAL DOM NODES  ////////////


function text(string)
{
	return {
		type: 'text',
		text: string
	};
}


function node(tag)
{
	return F2(function(factList, kidList) {
		return nodeHelp(tag, factList, kidList);
	});
}


function nodeHelp(tag, factList, kidList)
{
	var organized = organizeFacts(factList);
	var namespace = organized.namespace;
	var facts = organized.facts;

	var children = [];
	var descendantsCount = 0;
	while (kidList.ctor !== '[]')
	{
		var kid = kidList._0;
		descendantsCount += (kid.descendantsCount || 0);
		children.push(kid);
		kidList = kidList._1;
	}
	descendantsCount += children.length;

	return {
		type: 'node',
		tag: tag,
		facts: facts,
		children: children,
		namespace: namespace,
		descendantsCount: descendantsCount
	};
}


function keyedNode(tag, factList, kidList)
{
	var organized = organizeFacts(factList);
	var namespace = organized.namespace;
	var facts = organized.facts;

	var children = [];
	var descendantsCount = 0;
	while (kidList.ctor !== '[]')
	{
		var kid = kidList._0;
		descendantsCount += (kid._1.descendantsCount || 0);
		children.push(kid);
		kidList = kidList._1;
	}
	descendantsCount += children.length;

	return {
		type: 'keyed-node',
		tag: tag,
		facts: facts,
		children: children,
		namespace: namespace,
		descendantsCount: descendantsCount
	};
}


function custom(factList, model, impl)
{
	var facts = organizeFacts(factList).facts;

	return {
		type: 'custom',
		facts: facts,
		model: model,
		impl: impl
	};
}


function map(tagger, node)
{
	return {
		type: 'tagger',
		tagger: tagger,
		node: node,
		descendantsCount: 1 + (node.descendantsCount || 0)
	};
}


function thunk(func, args, thunk)
{
	return {
		type: 'thunk',
		func: func,
		args: args,
		thunk: thunk,
		node: undefined
	};
}

function lazy(fn, a)
{
	return thunk(fn, [a], function() {
		return fn(a);
	});
}

function lazy2(fn, a, b)
{
	return thunk(fn, [a,b], function() {
		return A2(fn, a, b);
	});
}

function lazy3(fn, a, b, c)
{
	return thunk(fn, [a,b,c], function() {
		return A3(fn, a, b, c);
	});
}



// FACTS


function organizeFacts(factList)
{
	var namespace, facts = {};

	while (factList.ctor !== '[]')
	{
		var entry = factList._0;
		var key = entry.key;

		if (key === ATTR_KEY || key === ATTR_NS_KEY || key === EVENT_KEY)
		{
			var subFacts = facts[key] || {};
			subFacts[entry.realKey] = entry.value;
			facts[key] = subFacts;
		}
		else if (key === STYLE_KEY)
		{
			var styles = facts[key] || {};
			var styleList = entry.value;
			while (styleList.ctor !== '[]')
			{
				var style = styleList._0;
				styles[style._0] = style._1;
				styleList = styleList._1;
			}
			facts[key] = styles;
		}
		else if (key === 'namespace')
		{
			namespace = entry.value;
		}
		else if (key === 'className')
		{
			var classes = facts[key];
			facts[key] = typeof classes === 'undefined'
				? entry.value
				: classes + ' ' + entry.value;
		}
 		else
		{
			facts[key] = entry.value;
		}
		factList = factList._1;
	}

	return {
		facts: facts,
		namespace: namespace
	};
}



////////////  PROPERTIES AND ATTRIBUTES  ////////////


function style(value)
{
	return {
		key: STYLE_KEY,
		value: value
	};
}


function property(key, value)
{
	return {
		key: key,
		value: value
	};
}


function attribute(key, value)
{
	return {
		key: ATTR_KEY,
		realKey: key,
		value: value
	};
}


function attributeNS(namespace, key, value)
{
	return {
		key: ATTR_NS_KEY,
		realKey: key,
		value: {
			value: value,
			namespace: namespace
		}
	};
}


function on(name, options, decoder)
{
	return {
		key: EVENT_KEY,
		realKey: name,
		value: {
			options: options,
			decoder: decoder
		}
	};
}


function equalEvents(a, b)
{
	if (a.options !== b.options)
	{
		if (a.options.stopPropagation !== b.options.stopPropagation || a.options.preventDefault !== b.options.preventDefault)
		{
			return false;
		}
	}
	return _elm_lang$core$Native_Json.equality(a.decoder, b.decoder);
}


function mapProperty(func, property)
{
	if (property.key !== EVENT_KEY)
	{
		return property;
	}
	return on(
		property.realKey,
		property.value.options,
		A2(_elm_lang$core$Json_Decode$map, func, property.value.decoder)
	);
}


////////////  RENDER  ////////////


function render(vNode, eventNode)
{
	switch (vNode.type)
	{
		case 'thunk':
			if (!vNode.node)
			{
				vNode.node = vNode.thunk();
			}
			return render(vNode.node, eventNode);

		case 'tagger':
			var subNode = vNode.node;
			var tagger = vNode.tagger;

			while (subNode.type === 'tagger')
			{
				typeof tagger !== 'object'
					? tagger = [tagger, subNode.tagger]
					: tagger.push(subNode.tagger);

				subNode = subNode.node;
			}

			var subEventRoot = { tagger: tagger, parent: eventNode };
			var domNode = render(subNode, subEventRoot);
			domNode.elm_event_node_ref = subEventRoot;
			return domNode;

		case 'text':
			return localDoc.createTextNode(vNode.text);

		case 'node':
			var domNode = vNode.namespace
				? localDoc.createElementNS(vNode.namespace, vNode.tag)
				: localDoc.createElement(vNode.tag);

			applyFacts(domNode, eventNode, vNode.facts);

			var children = vNode.children;

			for (var i = 0; i < children.length; i++)
			{
				domNode.appendChild(render(children[i], eventNode));
			}

			return domNode;

		case 'keyed-node':
			var domNode = vNode.namespace
				? localDoc.createElementNS(vNode.namespace, vNode.tag)
				: localDoc.createElement(vNode.tag);

			applyFacts(domNode, eventNode, vNode.facts);

			var children = vNode.children;

			for (var i = 0; i < children.length; i++)
			{
				domNode.appendChild(render(children[i]._1, eventNode));
			}

			return domNode;

		case 'custom':
			var domNode = vNode.impl.render(vNode.model);
			applyFacts(domNode, eventNode, vNode.facts);
			return domNode;
	}
}



////////////  APPLY FACTS  ////////////


function applyFacts(domNode, eventNode, facts)
{
	for (var key in facts)
	{
		var value = facts[key];

		switch (key)
		{
			case STYLE_KEY:
				applyStyles(domNode, value);
				break;

			case EVENT_KEY:
				applyEvents(domNode, eventNode, value);
				break;

			case ATTR_KEY:
				applyAttrs(domNode, value);
				break;

			case ATTR_NS_KEY:
				applyAttrsNS(domNode, value);
				break;

			case 'value':
				if (domNode[key] !== value)
				{
					domNode[key] = value;
				}
				break;

			default:
				domNode[key] = value;
				break;
		}
	}
}

function applyStyles(domNode, styles)
{
	var domNodeStyle = domNode.style;

	for (var key in styles)
	{
		domNodeStyle[key] = styles[key];
	}
}

function applyEvents(domNode, eventNode, events)
{
	var allHandlers = domNode.elm_handlers || {};

	for (var key in events)
	{
		var handler = allHandlers[key];
		var value = events[key];

		if (typeof value === 'undefined')
		{
			domNode.removeEventListener(key, handler);
			allHandlers[key] = undefined;
		}
		else if (typeof handler === 'undefined')
		{
			var handler = makeEventHandler(eventNode, value);
			domNode.addEventListener(key, handler);
			allHandlers[key] = handler;
		}
		else
		{
			handler.info = value;
		}
	}

	domNode.elm_handlers = allHandlers;
}

function makeEventHandler(eventNode, info)
{
	function eventHandler(event)
	{
		var info = eventHandler.info;

		var value = A2(_elm_lang$core$Native_Json.run, info.decoder, event);

		if (value.ctor === 'Ok')
		{
			var options = info.options;
			if (options.stopPropagation)
			{
				event.stopPropagation();
			}
			if (options.preventDefault)
			{
				event.preventDefault();
			}

			var message = value._0;

			var currentEventNode = eventNode;
			while (currentEventNode)
			{
				var tagger = currentEventNode.tagger;
				if (typeof tagger === 'function')
				{
					message = tagger(message);
				}
				else
				{
					for (var i = tagger.length; i--; )
					{
						message = tagger[i](message);
					}
				}
				currentEventNode = currentEventNode.parent;
			}
		}
	};

	eventHandler.info = info;

	return eventHandler;
}

function applyAttrs(domNode, attrs)
{
	for (var key in attrs)
	{
		var value = attrs[key];
		if (typeof value === 'undefined')
		{
			domNode.removeAttribute(key);
		}
		else
		{
			domNode.setAttribute(key, value);
		}
	}
}

function applyAttrsNS(domNode, nsAttrs)
{
	for (var key in nsAttrs)
	{
		var pair = nsAttrs[key];
		var namespace = pair.namespace;
		var value = pair.value;

		if (typeof value === 'undefined')
		{
			domNode.removeAttributeNS(namespace, key);
		}
		else
		{
			domNode.setAttributeNS(namespace, key, value);
		}
	}
}



////////////  DIFF  ////////////


function diff(a, b)
{
	var patches = [];
	diffHelp(a, b, patches, 0);
	return patches;
}


function makePatch(type, index, data)
{
	return {
		index: index,
		type: type,
		data: data,
		domNode: undefined,
		eventNode: undefined
	};
}


function diffHelp(a, b, patches, index)
{
	if (a === b)
	{
		return;
	}

	var aType = a.type;
	var bType = b.type;

	// Bail if you run into different types of nodes. Implies that the
	// structure has changed significantly and it's not worth a diff.
	if (aType !== bType)
	{
		patches.push(makePatch('p-redraw', index, b));
		return;
	}

	// Now we know that both nodes are the same type.
	switch (bType)
	{
		case 'thunk':
			var aArgs = a.args;
			var bArgs = b.args;
			var i = aArgs.length;
			var same = a.func === b.func && i === bArgs.length;
			while (same && i--)
			{
				same = aArgs[i] === bArgs[i];
			}
			if (same)
			{
				b.node = a.node;
				return;
			}
			b.node = b.thunk();
			var subPatches = [];
			diffHelp(a.node, b.node, subPatches, 0);
			if (subPatches.length > 0)
			{
				patches.push(makePatch('p-thunk', index, subPatches));
			}
			return;

		case 'tagger':
			// gather nested taggers
			var aTaggers = a.tagger;
			var bTaggers = b.tagger;
			var nesting = false;

			var aSubNode = a.node;
			while (aSubNode.type === 'tagger')
			{
				nesting = true;

				typeof aTaggers !== 'object'
					? aTaggers = [aTaggers, aSubNode.tagger]
					: aTaggers.push(aSubNode.tagger);

				aSubNode = aSubNode.node;
			}

			var bSubNode = b.node;
			while (bSubNode.type === 'tagger')
			{
				nesting = true;

				typeof bTaggers !== 'object'
					? bTaggers = [bTaggers, bSubNode.tagger]
					: bTaggers.push(bSubNode.tagger);

				bSubNode = bSubNode.node;
			}

			// Just bail if different numbers of taggers. This implies the
			// structure of the virtual DOM has changed.
			if (nesting && aTaggers.length !== bTaggers.length)
			{
				patches.push(makePatch('p-redraw', index, b));
				return;
			}

			// check if taggers are "the same"
			if (nesting ? !pairwiseRefEqual(aTaggers, bTaggers) : aTaggers !== bTaggers)
			{
				patches.push(makePatch('p-tagger', index, bTaggers));
			}

			// diff everything below the taggers
			diffHelp(aSubNode, bSubNode, patches, index + 1);
			return;

		case 'text':
			if (a.text !== b.text)
			{
				patches.push(makePatch('p-text', index, b.text));
				return;
			}

			return;

		case 'node':
			// Bail if obvious indicators have changed. Implies more serious
			// structural changes such that it's not worth it to diff.
			if (a.tag !== b.tag || a.namespace !== b.namespace)
			{
				patches.push(makePatch('p-redraw', index, b));
				return;
			}

			var factsDiff = diffFacts(a.facts, b.facts);

			if (typeof factsDiff !== 'undefined')
			{
				patches.push(makePatch('p-facts', index, factsDiff));
			}

			diffChildren(a, b, patches, index);
			return;

		case 'keyed-node':
			// Bail if obvious indicators have changed. Implies more serious
			// structural changes such that it's not worth it to diff.
			if (a.tag !== b.tag || a.namespace !== b.namespace)
			{
				patches.push(makePatch('p-redraw', index, b));
				return;
			}

			var factsDiff = diffFacts(a.facts, b.facts);

			if (typeof factsDiff !== 'undefined')
			{
				patches.push(makePatch('p-facts', index, factsDiff));
			}

			diffKeyedChildren(a, b, patches, index);
			return;

		case 'custom':
			if (a.impl !== b.impl)
			{
				patches.push(makePatch('p-redraw', index, b));
				return;
			}

			var factsDiff = diffFacts(a.facts, b.facts);
			if (typeof factsDiff !== 'undefined')
			{
				patches.push(makePatch('p-facts', index, factsDiff));
			}

			var patch = b.impl.diff(a,b);
			if (patch)
			{
				patches.push(makePatch('p-custom', index, patch));
				return;
			}

			return;
	}
}


// assumes the incoming arrays are the same length
function pairwiseRefEqual(as, bs)
{
	for (var i = 0; i < as.length; i++)
	{
		if (as[i] !== bs[i])
		{
			return false;
		}
	}

	return true;
}


// TODO Instead of creating a new diff object, it's possible to just test if
// there *is* a diff. During the actual patch, do the diff again and make the
// modifications directly. This way, there's no new allocations. Worth it?
function diffFacts(a, b, category)
{
	var diff;

	// look for changes and removals
	for (var aKey in a)
	{
		if (aKey === STYLE_KEY || aKey === EVENT_KEY || aKey === ATTR_KEY || aKey === ATTR_NS_KEY)
		{
			var subDiff = diffFacts(a[aKey], b[aKey] || {}, aKey);
			if (subDiff)
			{
				diff = diff || {};
				diff[aKey] = subDiff;
			}
			continue;
		}

		// remove if not in the new facts
		if (!(aKey in b))
		{
			diff = diff || {};
			diff[aKey] =
				(typeof category === 'undefined')
					? (typeof a[aKey] === 'string' ? '' : null)
					:
				(category === STYLE_KEY)
					? ''
					:
				(category === EVENT_KEY || category === ATTR_KEY)
					? undefined
					:
				{ namespace: a[aKey].namespace, value: undefined };

			continue;
		}

		var aValue = a[aKey];
		var bValue = b[aKey];

		// reference equal, so don't worry about it
		if (aValue === bValue && aKey !== 'value'
			|| category === EVENT_KEY && equalEvents(aValue, bValue))
		{
			continue;
		}

		diff = diff || {};
		diff[aKey] = bValue;
	}

	// add new stuff
	for (var bKey in b)
	{
		if (!(bKey in a))
		{
			diff = diff || {};
			diff[bKey] = b[bKey];
		}
	}

	return diff;
}


function diffChildren(aParent, bParent, patches, rootIndex)
{
	var aChildren = aParent.children;
	var bChildren = bParent.children;

	var aLen = aChildren.length;
	var bLen = bChildren.length;

	// FIGURE OUT IF THERE ARE INSERTS OR REMOVALS

	if (aLen > bLen)
	{
		patches.push(makePatch('p-remove-last', rootIndex, aLen - bLen));
	}
	else if (aLen < bLen)
	{
		patches.push(makePatch('p-append', rootIndex, bChildren.slice(aLen)));
	}

	// PAIRWISE DIFF EVERYTHING ELSE

	var index = rootIndex;
	var minLen = aLen < bLen ? aLen : bLen;
	for (var i = 0; i < minLen; i++)
	{
		index++;
		var aChild = aChildren[i];
		diffHelp(aChild, bChildren[i], patches, index);
		index += aChild.descendantsCount || 0;
	}
}



////////////  KEYED DIFF  ////////////


function diffKeyedChildren(aParent, bParent, patches, rootIndex)
{
	var localPatches = [];

	var changes = {}; // Dict String Entry
	var inserts = []; // Array { index : Int, entry : Entry }
	// type Entry = { tag : String, vnode : VNode, index : Int, data : _ }

	var aChildren = aParent.children;
	var bChildren = bParent.children;
	var aLen = aChildren.length;
	var bLen = bChildren.length;
	var aIndex = 0;
	var bIndex = 0;

	var index = rootIndex;

	while (aIndex < aLen && bIndex < bLen)
	{
		var a = aChildren[aIndex];
		var b = bChildren[bIndex];

		var aKey = a._0;
		var bKey = b._0;
		var aNode = a._1;
		var bNode = b._1;

		// check if keys match

		if (aKey === bKey)
		{
			index++;
			diffHelp(aNode, bNode, localPatches, index);
			index += aNode.descendantsCount || 0;

			aIndex++;
			bIndex++;
			continue;
		}

		// look ahead 1 to detect insertions and removals.

		var aLookAhead = aIndex + 1 < aLen;
		var bLookAhead = bIndex + 1 < bLen;

		if (aLookAhead)
		{
			var aNext = aChildren[aIndex + 1];
			var aNextKey = aNext._0;
			var aNextNode = aNext._1;
			var oldMatch = bKey === aNextKey;
		}

		if (bLookAhead)
		{
			var bNext = bChildren[bIndex + 1];
			var bNextKey = bNext._0;
			var bNextNode = bNext._1;
			var newMatch = aKey === bNextKey;
		}


		// swap a and b
		if (aLookAhead && bLookAhead && newMatch && oldMatch)
		{
			index++;
			diffHelp(aNode, bNextNode, localPatches, index);
			insertNode(changes, localPatches, aKey, bNode, bIndex, inserts);
			index += aNode.descendantsCount || 0;

			index++;
			removeNode(changes, localPatches, aKey, aNextNode, index);
			index += aNextNode.descendantsCount || 0;

			aIndex += 2;
			bIndex += 2;
			continue;
		}

		// insert b
		if (bLookAhead && newMatch)
		{
			index++;
			insertNode(changes, localPatches, bKey, bNode, bIndex, inserts);
			diffHelp(aNode, bNextNode, localPatches, index);
			index += aNode.descendantsCount || 0;

			aIndex += 1;
			bIndex += 2;
			continue;
		}

		// remove a
		if (aLookAhead && oldMatch)
		{
			index++;
			removeNode(changes, localPatches, aKey, aNode, index);
			index += aNode.descendantsCount || 0;

			index++;
			diffHelp(aNextNode, bNode, localPatches, index);
			index += aNextNode.descendantsCount || 0;

			aIndex += 2;
			bIndex += 1;
			continue;
		}

		// remove a, insert b
		if (aLookAhead && bLookAhead && aNextKey === bNextKey)
		{
			index++;
			removeNode(changes, localPatches, aKey, aNode, index);
			insertNode(changes, localPatches, bKey, bNode, bIndex, inserts);
			index += aNode.descendantsCount || 0;

			index++;
			diffHelp(aNextNode, bNextNode, localPatches, index);
			index += aNextNode.descendantsCount || 0;

			aIndex += 2;
			bIndex += 2;
			continue;
		}

		break;
	}

	// eat up any remaining nodes with removeNode and insertNode

	while (aIndex < aLen)
	{
		index++;
		var a = aChildren[aIndex];
		var aNode = a._1;
		removeNode(changes, localPatches, a._0, aNode, index);
		index += aNode.descendantsCount || 0;
		aIndex++;
	}

	var endInserts;
	while (bIndex < bLen)
	{
		endInserts = endInserts || [];
		var b = bChildren[bIndex];
		insertNode(changes, localPatches, b._0, b._1, undefined, endInserts);
		bIndex++;
	}

	if (localPatches.length > 0 || inserts.length > 0 || typeof endInserts !== 'undefined')
	{
		patches.push(makePatch('p-reorder', rootIndex, {
			patches: localPatches,
			inserts: inserts,
			endInserts: endInserts
		}));
	}
}



////////////  CHANGES FROM KEYED DIFF  ////////////


var POSTFIX = '_elmW6BL';


function insertNode(changes, localPatches, key, vnode, bIndex, inserts)
{
	var entry = changes[key];

	// never seen this key before
	if (typeof entry === 'undefined')
	{
		entry = {
			tag: 'insert',
			vnode: vnode,
			index: bIndex,
			data: undefined
		};

		inserts.push({ index: bIndex, entry: entry });
		changes[key] = entry;

		return;
	}

	// this key was removed earlier, a match!
	if (entry.tag === 'remove')
	{
		inserts.push({ index: bIndex, entry: entry });

		entry.tag = 'move';
		var subPatches = [];
		diffHelp(entry.vnode, vnode, subPatches, entry.index);
		entry.index = bIndex;
		entry.data.data = {
			patches: subPatches,
			entry: entry
		};

		return;
	}

	// this key has already been inserted or moved, a duplicate!
	insertNode(changes, localPatches, key + POSTFIX, vnode, bIndex, inserts);
}


function removeNode(changes, localPatches, key, vnode, index)
{
	var entry = changes[key];

	// never seen this key before
	if (typeof entry === 'undefined')
	{
		var patch = makePatch('p-remove', index, undefined);
		localPatches.push(patch);

		changes[key] = {
			tag: 'remove',
			vnode: vnode,
			index: index,
			data: patch
		};

		return;
	}

	// this key was inserted earlier, a match!
	if (entry.tag === 'insert')
	{
		entry.tag = 'move';
		var subPatches = [];
		diffHelp(vnode, entry.vnode, subPatches, index);

		var patch = makePatch('p-remove', index, {
			patches: subPatches,
			entry: entry
		});
		localPatches.push(patch);

		return;
	}

	// this key has already been removed or moved, a duplicate!
	removeNode(changes, localPatches, key + POSTFIX, vnode, index);
}



////////////  ADD DOM NODES  ////////////
//
// Each DOM node has an "index" assigned in order of traversal. It is important
// to minimize our crawl over the actual DOM, so these indexes (along with the
// descendantsCount of virtual nodes) let us skip touching entire subtrees of
// the DOM if we know there are no patches there.


function addDomNodes(domNode, vNode, patches, eventNode)
{
	addDomNodesHelp(domNode, vNode, patches, 0, 0, vNode.descendantsCount, eventNode);
}


// assumes `patches` is non-empty and indexes increase monotonically.
function addDomNodesHelp(domNode, vNode, patches, i, low, high, eventNode)
{
	var patch = patches[i];
	var index = patch.index;

	while (index === low)
	{
		var patchType = patch.type;

		if (patchType === 'p-thunk')
		{
			addDomNodes(domNode, vNode.node, patch.data, eventNode);
		}
		else if (patchType === 'p-reorder')
		{
			patch.domNode = domNode;
			patch.eventNode = eventNode;

			var subPatches = patch.data.patches;
			if (subPatches.length > 0)
			{
				addDomNodesHelp(domNode, vNode, subPatches, 0, low, high, eventNode);
			}
		}
		else if (patchType === 'p-remove')
		{
			patch.domNode = domNode;
			patch.eventNode = eventNode;

			var data = patch.data;
			if (typeof data !== 'undefined')
			{
				data.entry.data = domNode;
				var subPatches = data.patches;
				if (subPatches.length > 0)
				{
					addDomNodesHelp(domNode, vNode, subPatches, 0, low, high, eventNode);
				}
			}
		}
		else
		{
			patch.domNode = domNode;
			patch.eventNode = eventNode;
		}

		i++;

		if (!(patch = patches[i]) || (index = patch.index) > high)
		{
			return i;
		}
	}

	switch (vNode.type)
	{
		case 'tagger':
			var subNode = vNode.node;

			while (subNode.type === "tagger")
			{
				subNode = subNode.node;
			}

			return addDomNodesHelp(domNode, subNode, patches, i, low + 1, high, domNode.elm_event_node_ref);

		case 'node':
			var vChildren = vNode.children;
			var childNodes = domNode.childNodes;
			for (var j = 0; j < vChildren.length; j++)
			{
				low++;
				var vChild = vChildren[j];
				var nextLow = low + (vChild.descendantsCount || 0);
				if (low <= index && index <= nextLow)
				{
					i = addDomNodesHelp(childNodes[j], vChild, patches, i, low, nextLow, eventNode);
					if (!(patch = patches[i]) || (index = patch.index) > high)
					{
						return i;
					}
				}
				low = nextLow;
			}
			return i;

		case 'keyed-node':
			var vChildren = vNode.children;
			var childNodes = domNode.childNodes;
			for (var j = 0; j < vChildren.length; j++)
			{
				low++;
				var vChild = vChildren[j]._1;
				var nextLow = low + (vChild.descendantsCount || 0);
				if (low <= index && index <= nextLow)
				{
					i = addDomNodesHelp(childNodes[j], vChild, patches, i, low, nextLow, eventNode);
					if (!(patch = patches[i]) || (index = patch.index) > high)
					{
						return i;
					}
				}
				low = nextLow;
			}
			return i;

		case 'text':
		case 'thunk':
			throw new Error('should never traverse `text` or `thunk` nodes like this');
	}
}



////////////  APPLY PATCHES  ////////////


function applyPatches(rootDomNode, oldVirtualNode, patches, eventNode)
{
	if (patches.length === 0)
	{
		return rootDomNode;
	}

	addDomNodes(rootDomNode, oldVirtualNode, patches, eventNode);
	return applyPatchesHelp(rootDomNode, patches);
}

function applyPatchesHelp(rootDomNode, patches)
{
	for (var i = 0; i < patches.length; i++)
	{
		var patch = patches[i];
		var localDomNode = patch.domNode
		var newNode = applyPatch(localDomNode, patch);
		if (localDomNode === rootDomNode)
		{
			rootDomNode = newNode;
		}
	}
	return rootDomNode;
}

function applyPatch(domNode, patch)
{
	switch (patch.type)
	{
		case 'p-redraw':
			return applyPatchRedraw(domNode, patch.data, patch.eventNode);

		case 'p-facts':
			applyFacts(domNode, patch.eventNode, patch.data);
			return domNode;

		case 'p-text':
			domNode.replaceData(0, domNode.length, patch.data);
			return domNode;

		case 'p-thunk':
			return applyPatchesHelp(domNode, patch.data);

		case 'p-tagger':
			if (typeof domNode.elm_event_node_ref !== 'undefined')
			{
				domNode.elm_event_node_ref.tagger = patch.data;
			}
			else
			{
				domNode.elm_event_node_ref = { tagger: patch.data, parent: patch.eventNode };
			}
			return domNode;

		case 'p-remove-last':
			var i = patch.data;
			while (i--)
			{
				domNode.removeChild(domNode.lastChild);
			}
			return domNode;

		case 'p-append':
			var newNodes = patch.data;
			for (var i = 0; i < newNodes.length; i++)
			{
				domNode.appendChild(render(newNodes[i], patch.eventNode));
			}
			return domNode;

		case 'p-remove':
			var data = patch.data;
			if (typeof data === 'undefined')
			{
				domNode.parentNode.removeChild(domNode);
				return domNode;
			}
			var entry = data.entry;
			if (typeof entry.index !== 'undefined')
			{
				domNode.parentNode.removeChild(domNode);
			}
			entry.data = applyPatchesHelp(domNode, data.patches);
			return domNode;

		case 'p-reorder':
			return applyPatchReorder(domNode, patch);

		case 'p-custom':
			var impl = patch.data;
			return impl.applyPatch(domNode, impl.data);

		default:
			throw new Error('Ran into an unknown patch!');
	}
}


function applyPatchRedraw(domNode, vNode, eventNode)
{
	var parentNode = domNode.parentNode;
	var newNode = render(vNode, eventNode);

	if (typeof newNode.elm_event_node_ref === 'undefined')
	{
		newNode.elm_event_node_ref = domNode.elm_event_node_ref;
	}

	if (parentNode && newNode !== domNode)
	{
		parentNode.replaceChild(newNode, domNode);
	}
	return newNode;
}


function applyPatchReorder(domNode, patch)
{
	var data = patch.data;

	// remove end inserts
	var frag = applyPatchReorderEndInsertsHelp(data.endInserts, patch);

	// removals
	domNode = applyPatchesHelp(domNode, data.patches);

	// inserts
	var inserts = data.inserts;
	for (var i = 0; i < inserts.length; i++)
	{
		var insert = inserts[i];
		var entry = insert.entry;
		var node = entry.tag === 'move'
			? entry.data
			: render(entry.vnode, patch.eventNode);
		domNode.insertBefore(node, domNode.childNodes[insert.index]);
	}

	// add end inserts
	if (typeof frag !== 'undefined')
	{
		domNode.appendChild(frag);
	}

	return domNode;
}


function applyPatchReorderEndInsertsHelp(endInserts, patch)
{
	if (typeof endInserts === 'undefined')
	{
		return;
	}

	var frag = localDoc.createDocumentFragment();
	for (var i = 0; i < endInserts.length; i++)
	{
		var insert = endInserts[i];
		var entry = insert.entry;
		frag.appendChild(entry.tag === 'move'
			? entry.data
			: render(entry.vnode, patch.eventNode)
		);
	}
	return frag;
}


// PROGRAMS

var program = makeProgram(checkNoFlags);
var programWithFlags = makeProgram(checkYesFlags);

function makeProgram(flagChecker)
{
	return F2(function(debugWrap, impl)
	{
		return function(flagDecoder)
		{
			return function(object, moduleName, debugMetadata)
			{
				var checker = flagChecker(flagDecoder, moduleName);
				if (typeof debugMetadata === 'undefined')
				{
					normalSetup(impl, object, moduleName, checker);
				}
				else
				{
					debugSetup(A2(debugWrap, debugMetadata, impl), object, moduleName, checker);
				}
			};
		};
	});
}

function staticProgram(vNode)
{
	var nothing = _elm_lang$core$Native_Utils.Tuple2(
		_elm_lang$core$Native_Utils.Tuple0,
		_elm_lang$core$Platform_Cmd$none
	);
	return A2(program, _elm_lang$virtual_dom$VirtualDom_Debug$wrap, {
		init: nothing,
		view: function() { return vNode; },
		update: F2(function() { return nothing; }),
		subscriptions: function() { return _elm_lang$core$Platform_Sub$none; }
	})();
}


// FLAG CHECKERS

function checkNoFlags(flagDecoder, moduleName)
{
	return function(init, flags, domNode)
	{
		if (typeof flags === 'undefined')
		{
			return init;
		}

		var errorMessage =
			'The `' + moduleName + '` module does not need flags.\n'
			+ 'Initialize it with no arguments and you should be all set!';

		crash(errorMessage, domNode);
	};
}

function checkYesFlags(flagDecoder, moduleName)
{
	return function(init, flags, domNode)
	{
		if (typeof flagDecoder === 'undefined')
		{
			var errorMessage =
				'Are you trying to sneak a Never value into Elm? Trickster!\n'
				+ 'It looks like ' + moduleName + '.main is defined with `programWithFlags` but has type `Program Never`.\n'
				+ 'Use `program` instead if you do not want flags.'

			crash(errorMessage, domNode);
		}

		var result = A2(_elm_lang$core$Native_Json.run, flagDecoder, flags);
		if (result.ctor === 'Ok')
		{
			return init(result._0);
		}

		var errorMessage =
			'Trying to initialize the `' + moduleName + '` module with an unexpected flag.\n'
			+ 'I tried to convert it to an Elm value, but ran into this problem:\n\n'
			+ result._0;

		crash(errorMessage, domNode);
	};
}

function crash(errorMessage, domNode)
{
	if (domNode)
	{
		domNode.innerHTML =
			'<div style="padding-left:1em;">'
			+ '<h2 style="font-weight:normal;"><b>Oops!</b> Something went wrong when starting your Elm program.</h2>'
			+ '<pre style="padding-left:1em;">' + errorMessage + '</pre>'
			+ '</div>';
	}

	throw new Error(errorMessage);
}


//  NORMAL SETUP

function normalSetup(impl, object, moduleName, flagChecker)
{
	object['embed'] = function embed(node, flags)
	{
		while (node.lastChild)
		{
			node.removeChild(node.lastChild);
		}

		return _elm_lang$core$Native_Platform.initialize(
			flagChecker(impl.init, flags, node),
			impl.update,
			impl.subscriptions,
			normalRenderer(node, impl.view)
		);
	};

	object['fullscreen'] = function fullscreen(flags)
	{
		return _elm_lang$core$Native_Platform.initialize(
			flagChecker(impl.init, flags, document.body),
			impl.update,
			impl.subscriptions,
			normalRenderer(document.body, impl.view)
		);
	};
}

function normalRenderer(parentNode, view)
{
	return function(tagger, initialModel)
	{
		var eventNode = { tagger: tagger, parent: undefined };
		var initialVirtualNode = view(initialModel);
		var domNode = render(initialVirtualNode, eventNode);
		parentNode.appendChild(domNode);
		return makeStepper(domNode, view, initialVirtualNode, eventNode);
	};
}


// STEPPER

var rAF =
	typeof requestAnimationFrame !== 'undefined'
		? requestAnimationFrame
		: function(callback) { setTimeout(callback, 1000 / 60); };

function makeStepper(domNode, view, initialVirtualNode, eventNode)
{
	var state = 'NO_REQUEST';
	var currNode = initialVirtualNode;
	var nextModel;

	function updateIfNeeded()
	{
		switch (state)
		{
			case 'NO_REQUEST':
				throw new Error(
					'Unexpected draw callback.\n' +
					'Please report this to <https://github.com/elm-lang/virtual-dom/issues>.'
				);

			case 'PENDING_REQUEST':
				rAF(updateIfNeeded);
				state = 'EXTRA_REQUEST';

				var nextNode = view(nextModel);
				var patches = diff(currNode, nextNode);
				domNode = applyPatches(domNode, currNode, patches, eventNode);
				currNode = nextNode;

				return;

			case 'EXTRA_REQUEST':
				state = 'NO_REQUEST';
				return;
		}
	}

	return function stepper(model)
	{
		if (state === 'NO_REQUEST')
		{
			rAF(updateIfNeeded);
		}
		state = 'PENDING_REQUEST';
		nextModel = model;
	};
}


// DEBUG SETUP

function debugSetup(impl, object, moduleName, flagChecker)
{
	object['fullscreen'] = function fullscreen(flags)
	{
		var popoutRef = { doc: undefined };
		return _elm_lang$core$Native_Platform.initialize(
			flagChecker(impl.init, flags, document.body),
			impl.update(scrollTask(popoutRef)),
			impl.subscriptions,
			debugRenderer(moduleName, document.body, popoutRef, impl.view, impl.viewIn, impl.viewOut)
		);
	};

	object['embed'] = function fullscreen(node, flags)
	{
		var popoutRef = { doc: undefined };
		return _elm_lang$core$Native_Platform.initialize(
			flagChecker(impl.init, flags, node),
			impl.update(scrollTask(popoutRef)),
			impl.subscriptions,
			debugRenderer(moduleName, node, popoutRef, impl.view, impl.viewIn, impl.viewOut)
		);
	};
}

function scrollTask(popoutRef)
{
	return _elm_lang$core$Native_Scheduler.nativeBinding(function(callback)
	{
		var doc = popoutRef.doc;
		if (doc)
		{
			var msgs = doc.getElementsByClassName('debugger-sidebar-messages')[0];
			if (msgs)
			{
				msgs.scrollTop = msgs.scrollHeight;
			}
		}
		callback(_elm_lang$core$Native_Scheduler.succeed(_elm_lang$core$Native_Utils.Tuple0));
	});
}


function debugRenderer(moduleName, parentNode, popoutRef, view, viewIn, viewOut)
{
	return function(tagger, initialModel)
	{
		var appEventNode = { tagger: tagger, parent: undefined };
		var eventNode = { tagger: tagger, parent: undefined };

		// make normal stepper
		var appVirtualNode = view(initialModel);
		var appNode = render(appVirtualNode, appEventNode);
		parentNode.appendChild(appNode);
		var appStepper = makeStepper(appNode, view, appVirtualNode, appEventNode);

		// make overlay stepper
		var overVirtualNode = viewIn(initialModel)._1;
		var overNode = render(overVirtualNode, eventNode);
		parentNode.appendChild(overNode);
		var wrappedViewIn = wrapViewIn(appEventNode, overNode, viewIn);
		var overStepper = makeStepper(overNode, wrappedViewIn, overVirtualNode, eventNode);

		// make debugger stepper
		var debugStepper = makeDebugStepper(initialModel, viewOut, eventNode, parentNode, moduleName, popoutRef);

		return function stepper(model)
		{
			appStepper(model);
			overStepper(model);
			debugStepper(model);
		}
	};
}

function makeDebugStepper(initialModel, view, eventNode, parentNode, moduleName, popoutRef)
{
	var curr;
	var domNode;

	return function stepper(model)
	{
		if (!model.isDebuggerOpen)
		{
			return;
		}

		if (!popoutRef.doc)
		{
			curr = view(model);
			domNode = openDebugWindow(moduleName, popoutRef, curr, eventNode);
			return;
		}

		// switch to document of popout
		localDoc = popoutRef.doc;

		var next = view(model);
		var patches = diff(curr, next);
		domNode = applyPatches(domNode, curr, patches, eventNode);
		curr = next;

		// switch back to normal document
		localDoc = document;
	};
}

function openDebugWindow(moduleName, popoutRef, virtualNode, eventNode)
{
	var w = 900;
	var h = 360;
	var x = screen.width - w;
	var y = screen.height - h;
	var debugWindow = window.open('', '', 'width=' + w + ',height=' + h + ',left=' + x + ',top=' + y);

	// switch to window document
	localDoc = debugWindow.document;

	popoutRef.doc = localDoc;
	localDoc.title = 'Debugger - ' + moduleName;
	localDoc.body.style.margin = '0';
	localDoc.body.style.padding = '0';
	var domNode = render(virtualNode, eventNode);
	localDoc.body.appendChild(domNode);

	localDoc.addEventListener('keydown', function(event) {
		if (event.metaKey && event.which === 82)
		{
			window.location.reload();
		}
		if (event.which === 38)
		{
			eventNode.tagger({ ctor: 'Up' });
			event.preventDefault();
		}
		if (event.which === 40)
		{
			eventNode.tagger({ ctor: 'Down' });
			event.preventDefault();
		}
	});

	function close()
	{
		popoutRef.doc = undefined;
		debugWindow.close();
	}
	window.addEventListener('unload', close);
	debugWindow.addEventListener('unload', function() {
		popoutRef.doc = undefined;
		window.removeEventListener('unload', close);
		eventNode.tagger({ ctor: 'Close' });
	});

	// switch back to the normal document
	localDoc = document;

	return domNode;
}


// BLOCK EVENTS

function wrapViewIn(appEventNode, overlayNode, viewIn)
{
	var ignorer = makeIgnorer(overlayNode);
	var blocking = 'Normal';
	var overflow;

	var normalTagger = appEventNode.tagger;
	var blockTagger = function() {};

	return function(model)
	{
		var tuple = viewIn(model);
		var newBlocking = tuple._0.ctor;
		appEventNode.tagger = newBlocking === 'Normal' ? normalTagger : blockTagger;
		if (blocking !== newBlocking)
		{
			traverse('removeEventListener', ignorer, blocking);
			traverse('addEventListener', ignorer, newBlocking);

			if (blocking === 'Normal')
			{
				overflow = document.body.style.overflow;
				document.body.style.overflow = 'hidden';
			}

			if (newBlocking === 'Normal')
			{
				document.body.style.overflow = overflow;
			}

			blocking = newBlocking;
		}
		return tuple._1;
	}
}

function traverse(verbEventListener, ignorer, blocking)
{
	switch(blocking)
	{
		case 'Normal':
			return;

		case 'Pause':
			return traverseHelp(verbEventListener, ignorer, mostEvents);

		case 'Message':
			return traverseHelp(verbEventListener, ignorer, allEvents);
	}
}

function traverseHelp(verbEventListener, handler, eventNames)
{
	for (var i = 0; i < eventNames.length; i++)
	{
		document.body[verbEventListener](eventNames[i], handler, true);
	}
}

function makeIgnorer(overlayNode)
{
	return function(event)
	{
		if (event.type === 'keydown' && event.metaKey && event.which === 82)
		{
			return;
		}

		var isScroll = event.type === 'scroll' || event.type === 'wheel';

		var node = event.target;
		while (node !== null)
		{
			if (node.className === 'elm-overlay-message-details' && isScroll)
			{
				return;
			}

			if (node === overlayNode && !isScroll)
			{
				return;
			}
			node = node.parentNode;
		}

		event.stopPropagation();
		event.preventDefault();
	}
}

var mostEvents = [
	'click', 'dblclick', 'mousemove',
	'mouseup', 'mousedown', 'mouseenter', 'mouseleave',
	'touchstart', 'touchend', 'touchcancel', 'touchmove',
	'pointerdown', 'pointerup', 'pointerover', 'pointerout',
	'pointerenter', 'pointerleave', 'pointermove', 'pointercancel',
	'dragstart', 'drag', 'dragend', 'dragenter', 'dragover', 'dragleave', 'drop',
	'keyup', 'keydown', 'keypress',
	'input', 'change',
	'focus', 'blur'
];

var allEvents = mostEvents.concat('wheel', 'scroll');


return {
	node: node,
	text: text,
	custom: custom,
	map: F2(map),

	on: F3(on),
	style: style,
	property: F2(property),
	attribute: F2(attribute),
	attributeNS: F3(attributeNS),
	mapProperty: F2(mapProperty),

	lazy: F2(lazy),
	lazy2: F3(lazy2),
	lazy3: F4(lazy3),
	keyedNode: F3(keyedNode),

	program: program,
	programWithFlags: programWithFlags,
	staticProgram: staticProgram
};

}();

var _elm_lang$virtual_dom$VirtualDom$programWithFlags = function (impl) {
	return A2(_elm_lang$virtual_dom$Native_VirtualDom.programWithFlags, _elm_lang$virtual_dom$VirtualDom_Debug$wrapWithFlags, impl);
};
var _elm_lang$virtual_dom$VirtualDom$program = function (impl) {
	return A2(_elm_lang$virtual_dom$Native_VirtualDom.program, _elm_lang$virtual_dom$VirtualDom_Debug$wrap, impl);
};
var _elm_lang$virtual_dom$VirtualDom$keyedNode = _elm_lang$virtual_dom$Native_VirtualDom.keyedNode;
var _elm_lang$virtual_dom$VirtualDom$lazy3 = _elm_lang$virtual_dom$Native_VirtualDom.lazy3;
var _elm_lang$virtual_dom$VirtualDom$lazy2 = _elm_lang$virtual_dom$Native_VirtualDom.lazy2;
var _elm_lang$virtual_dom$VirtualDom$lazy = _elm_lang$virtual_dom$Native_VirtualDom.lazy;
var _elm_lang$virtual_dom$VirtualDom$defaultOptions = {stopPropagation: false, preventDefault: false};
var _elm_lang$virtual_dom$VirtualDom$onWithOptions = _elm_lang$virtual_dom$Native_VirtualDom.on;
var _elm_lang$virtual_dom$VirtualDom$on = F2(
	function (eventName, decoder) {
		return A3(_elm_lang$virtual_dom$VirtualDom$onWithOptions, eventName, _elm_lang$virtual_dom$VirtualDom$defaultOptions, decoder);
	});
var _elm_lang$virtual_dom$VirtualDom$style = _elm_lang$virtual_dom$Native_VirtualDom.style;
var _elm_lang$virtual_dom$VirtualDom$mapProperty = _elm_lang$virtual_dom$Native_VirtualDom.mapProperty;
var _elm_lang$virtual_dom$VirtualDom$attributeNS = _elm_lang$virtual_dom$Native_VirtualDom.attributeNS;
var _elm_lang$virtual_dom$VirtualDom$attribute = _elm_lang$virtual_dom$Native_VirtualDom.attribute;
var _elm_lang$virtual_dom$VirtualDom$property = _elm_lang$virtual_dom$Native_VirtualDom.property;
var _elm_lang$virtual_dom$VirtualDom$map = _elm_lang$virtual_dom$Native_VirtualDom.map;
var _elm_lang$virtual_dom$VirtualDom$text = _elm_lang$virtual_dom$Native_VirtualDom.text;
var _elm_lang$virtual_dom$VirtualDom$node = _elm_lang$virtual_dom$Native_VirtualDom.node;
var _elm_lang$virtual_dom$VirtualDom$Options = F2(
	function (a, b) {
		return {stopPropagation: a, preventDefault: b};
	});
var _elm_lang$virtual_dom$VirtualDom$Node = {ctor: 'Node'};
var _elm_lang$virtual_dom$VirtualDom$Property = {ctor: 'Property'};

var _elm_lang$html$Html$programWithFlags = _elm_lang$virtual_dom$VirtualDom$programWithFlags;
var _elm_lang$html$Html$program = _elm_lang$virtual_dom$VirtualDom$program;
var _elm_lang$html$Html$beginnerProgram = function (_p0) {
	var _p1 = _p0;
	return _elm_lang$html$Html$program(
		{
			init: A2(
				_elm_lang$core$Platform_Cmd_ops['!'],
				_p1.model,
				{ctor: '[]'}),
			update: F2(
				function (msg, model) {
					return A2(
						_elm_lang$core$Platform_Cmd_ops['!'],
						A2(_p1.update, msg, model),
						{ctor: '[]'});
				}),
			view: _p1.view,
			subscriptions: function (_p2) {
				return _elm_lang$core$Platform_Sub$none;
			}
		});
};
var _elm_lang$html$Html$map = _elm_lang$virtual_dom$VirtualDom$map;
var _elm_lang$html$Html$text = _elm_lang$virtual_dom$VirtualDom$text;
var _elm_lang$html$Html$node = _elm_lang$virtual_dom$VirtualDom$node;
var _elm_lang$html$Html$body = _elm_lang$html$Html$node('body');
var _elm_lang$html$Html$section = _elm_lang$html$Html$node('section');
var _elm_lang$html$Html$nav = _elm_lang$html$Html$node('nav');
var _elm_lang$html$Html$article = _elm_lang$html$Html$node('article');
var _elm_lang$html$Html$aside = _elm_lang$html$Html$node('aside');
var _elm_lang$html$Html$h1 = _elm_lang$html$Html$node('h1');
var _elm_lang$html$Html$h2 = _elm_lang$html$Html$node('h2');
var _elm_lang$html$Html$h3 = _elm_lang$html$Html$node('h3');
var _elm_lang$html$Html$h4 = _elm_lang$html$Html$node('h4');
var _elm_lang$html$Html$h5 = _elm_lang$html$Html$node('h5');
var _elm_lang$html$Html$h6 = _elm_lang$html$Html$node('h6');
var _elm_lang$html$Html$header = _elm_lang$html$Html$node('header');
var _elm_lang$html$Html$footer = _elm_lang$html$Html$node('footer');
var _elm_lang$html$Html$address = _elm_lang$html$Html$node('address');
var _elm_lang$html$Html$main_ = _elm_lang$html$Html$node('main');
var _elm_lang$html$Html$p = _elm_lang$html$Html$node('p');
var _elm_lang$html$Html$hr = _elm_lang$html$Html$node('hr');
var _elm_lang$html$Html$pre = _elm_lang$html$Html$node('pre');
var _elm_lang$html$Html$blockquote = _elm_lang$html$Html$node('blockquote');
var _elm_lang$html$Html$ol = _elm_lang$html$Html$node('ol');
var _elm_lang$html$Html$ul = _elm_lang$html$Html$node('ul');
var _elm_lang$html$Html$li = _elm_lang$html$Html$node('li');
var _elm_lang$html$Html$dl = _elm_lang$html$Html$node('dl');
var _elm_lang$html$Html$dt = _elm_lang$html$Html$node('dt');
var _elm_lang$html$Html$dd = _elm_lang$html$Html$node('dd');
var _elm_lang$html$Html$figure = _elm_lang$html$Html$node('figure');
var _elm_lang$html$Html$figcaption = _elm_lang$html$Html$node('figcaption');
var _elm_lang$html$Html$div = _elm_lang$html$Html$node('div');
var _elm_lang$html$Html$a = _elm_lang$html$Html$node('a');
var _elm_lang$html$Html$em = _elm_lang$html$Html$node('em');
var _elm_lang$html$Html$strong = _elm_lang$html$Html$node('strong');
var _elm_lang$html$Html$small = _elm_lang$html$Html$node('small');
var _elm_lang$html$Html$s = _elm_lang$html$Html$node('s');
var _elm_lang$html$Html$cite = _elm_lang$html$Html$node('cite');
var _elm_lang$html$Html$q = _elm_lang$html$Html$node('q');
var _elm_lang$html$Html$dfn = _elm_lang$html$Html$node('dfn');
var _elm_lang$html$Html$abbr = _elm_lang$html$Html$node('abbr');
var _elm_lang$html$Html$time = _elm_lang$html$Html$node('time');
var _elm_lang$html$Html$code = _elm_lang$html$Html$node('code');
var _elm_lang$html$Html$var = _elm_lang$html$Html$node('var');
var _elm_lang$html$Html$samp = _elm_lang$html$Html$node('samp');
var _elm_lang$html$Html$kbd = _elm_lang$html$Html$node('kbd');
var _elm_lang$html$Html$sub = _elm_lang$html$Html$node('sub');
var _elm_lang$html$Html$sup = _elm_lang$html$Html$node('sup');
var _elm_lang$html$Html$i = _elm_lang$html$Html$node('i');
var _elm_lang$html$Html$b = _elm_lang$html$Html$node('b');
var _elm_lang$html$Html$u = _elm_lang$html$Html$node('u');
var _elm_lang$html$Html$mark = _elm_lang$html$Html$node('mark');
var _elm_lang$html$Html$ruby = _elm_lang$html$Html$node('ruby');
var _elm_lang$html$Html$rt = _elm_lang$html$Html$node('rt');
var _elm_lang$html$Html$rp = _elm_lang$html$Html$node('rp');
var _elm_lang$html$Html$bdi = _elm_lang$html$Html$node('bdi');
var _elm_lang$html$Html$bdo = _elm_lang$html$Html$node('bdo');
var _elm_lang$html$Html$span = _elm_lang$html$Html$node('span');
var _elm_lang$html$Html$br = _elm_lang$html$Html$node('br');
var _elm_lang$html$Html$wbr = _elm_lang$html$Html$node('wbr');
var _elm_lang$html$Html$ins = _elm_lang$html$Html$node('ins');
var _elm_lang$html$Html$del = _elm_lang$html$Html$node('del');
var _elm_lang$html$Html$img = _elm_lang$html$Html$node('img');
var _elm_lang$html$Html$iframe = _elm_lang$html$Html$node('iframe');
var _elm_lang$html$Html$embed = _elm_lang$html$Html$node('embed');
var _elm_lang$html$Html$object = _elm_lang$html$Html$node('object');
var _elm_lang$html$Html$param = _elm_lang$html$Html$node('param');
var _elm_lang$html$Html$video = _elm_lang$html$Html$node('video');
var _elm_lang$html$Html$audio = _elm_lang$html$Html$node('audio');
var _elm_lang$html$Html$source = _elm_lang$html$Html$node('source');
var _elm_lang$html$Html$track = _elm_lang$html$Html$node('track');
var _elm_lang$html$Html$canvas = _elm_lang$html$Html$node('canvas');
var _elm_lang$html$Html$math = _elm_lang$html$Html$node('math');
var _elm_lang$html$Html$table = _elm_lang$html$Html$node('table');
var _elm_lang$html$Html$caption = _elm_lang$html$Html$node('caption');
var _elm_lang$html$Html$colgroup = _elm_lang$html$Html$node('colgroup');
var _elm_lang$html$Html$col = _elm_lang$html$Html$node('col');
var _elm_lang$html$Html$tbody = _elm_lang$html$Html$node('tbody');
var _elm_lang$html$Html$thead = _elm_lang$html$Html$node('thead');
var _elm_lang$html$Html$tfoot = _elm_lang$html$Html$node('tfoot');
var _elm_lang$html$Html$tr = _elm_lang$html$Html$node('tr');
var _elm_lang$html$Html$td = _elm_lang$html$Html$node('td');
var _elm_lang$html$Html$th = _elm_lang$html$Html$node('th');
var _elm_lang$html$Html$form = _elm_lang$html$Html$node('form');
var _elm_lang$html$Html$fieldset = _elm_lang$html$Html$node('fieldset');
var _elm_lang$html$Html$legend = _elm_lang$html$Html$node('legend');
var _elm_lang$html$Html$label = _elm_lang$html$Html$node('label');
var _elm_lang$html$Html$input = _elm_lang$html$Html$node('input');
var _elm_lang$html$Html$button = _elm_lang$html$Html$node('button');
var _elm_lang$html$Html$select = _elm_lang$html$Html$node('select');
var _elm_lang$html$Html$datalist = _elm_lang$html$Html$node('datalist');
var _elm_lang$html$Html$optgroup = _elm_lang$html$Html$node('optgroup');
var _elm_lang$html$Html$option = _elm_lang$html$Html$node('option');
var _elm_lang$html$Html$textarea = _elm_lang$html$Html$node('textarea');
var _elm_lang$html$Html$keygen = _elm_lang$html$Html$node('keygen');
var _elm_lang$html$Html$output = _elm_lang$html$Html$node('output');
var _elm_lang$html$Html$progress = _elm_lang$html$Html$node('progress');
var _elm_lang$html$Html$meter = _elm_lang$html$Html$node('meter');
var _elm_lang$html$Html$details = _elm_lang$html$Html$node('details');
var _elm_lang$html$Html$summary = _elm_lang$html$Html$node('summary');
var _elm_lang$html$Html$menuitem = _elm_lang$html$Html$node('menuitem');
var _elm_lang$html$Html$menu = _elm_lang$html$Html$node('menu');

var _elm_lang$html$Html_Attributes$map = _elm_lang$virtual_dom$VirtualDom$mapProperty;
var _elm_lang$html$Html_Attributes$attribute = _elm_lang$virtual_dom$VirtualDom$attribute;
var _elm_lang$html$Html_Attributes$contextmenu = function (value) {
	return A2(_elm_lang$html$Html_Attributes$attribute, 'contextmenu', value);
};
var _elm_lang$html$Html_Attributes$draggable = function (value) {
	return A2(_elm_lang$html$Html_Attributes$attribute, 'draggable', value);
};
var _elm_lang$html$Html_Attributes$itemprop = function (value) {
	return A2(_elm_lang$html$Html_Attributes$attribute, 'itemprop', value);
};
var _elm_lang$html$Html_Attributes$tabindex = function (n) {
	return A2(
		_elm_lang$html$Html_Attributes$attribute,
		'tabIndex',
		_elm_lang$core$Basics$toString(n));
};
var _elm_lang$html$Html_Attributes$charset = function (value) {
	return A2(_elm_lang$html$Html_Attributes$attribute, 'charset', value);
};
var _elm_lang$html$Html_Attributes$height = function (value) {
	return A2(
		_elm_lang$html$Html_Attributes$attribute,
		'height',
		_elm_lang$core$Basics$toString(value));
};
var _elm_lang$html$Html_Attributes$width = function (value) {
	return A2(
		_elm_lang$html$Html_Attributes$attribute,
		'width',
		_elm_lang$core$Basics$toString(value));
};
var _elm_lang$html$Html_Attributes$formaction = function (value) {
	return A2(_elm_lang$html$Html_Attributes$attribute, 'formAction', value);
};
var _elm_lang$html$Html_Attributes$list = function (value) {
	return A2(_elm_lang$html$Html_Attributes$attribute, 'list', value);
};
var _elm_lang$html$Html_Attributes$minlength = function (n) {
	return A2(
		_elm_lang$html$Html_Attributes$attribute,
		'minLength',
		_elm_lang$core$Basics$toString(n));
};
var _elm_lang$html$Html_Attributes$maxlength = function (n) {
	return A2(
		_elm_lang$html$Html_Attributes$attribute,
		'maxlength',
		_elm_lang$core$Basics$toString(n));
};
var _elm_lang$html$Html_Attributes$size = function (n) {
	return A2(
		_elm_lang$html$Html_Attributes$attribute,
		'size',
		_elm_lang$core$Basics$toString(n));
};
var _elm_lang$html$Html_Attributes$form = function (value) {
	return A2(_elm_lang$html$Html_Attributes$attribute, 'form', value);
};
var _elm_lang$html$Html_Attributes$cols = function (n) {
	return A2(
		_elm_lang$html$Html_Attributes$attribute,
		'cols',
		_elm_lang$core$Basics$toString(n));
};
var _elm_lang$html$Html_Attributes$rows = function (n) {
	return A2(
		_elm_lang$html$Html_Attributes$attribute,
		'rows',
		_elm_lang$core$Basics$toString(n));
};
var _elm_lang$html$Html_Attributes$challenge = function (value) {
	return A2(_elm_lang$html$Html_Attributes$attribute, 'challenge', value);
};
var _elm_lang$html$Html_Attributes$media = function (value) {
	return A2(_elm_lang$html$Html_Attributes$attribute, 'media', value);
};
var _elm_lang$html$Html_Attributes$rel = function (value) {
	return A2(_elm_lang$html$Html_Attributes$attribute, 'rel', value);
};
var _elm_lang$html$Html_Attributes$datetime = function (value) {
	return A2(_elm_lang$html$Html_Attributes$attribute, 'datetime', value);
};
var _elm_lang$html$Html_Attributes$pubdate = function (value) {
	return A2(_elm_lang$html$Html_Attributes$attribute, 'pubdate', value);
};
var _elm_lang$html$Html_Attributes$colspan = function (n) {
	return A2(
		_elm_lang$html$Html_Attributes$attribute,
		'colspan',
		_elm_lang$core$Basics$toString(n));
};
var _elm_lang$html$Html_Attributes$rowspan = function (n) {
	return A2(
		_elm_lang$html$Html_Attributes$attribute,
		'rowspan',
		_elm_lang$core$Basics$toString(n));
};
var _elm_lang$html$Html_Attributes$manifest = function (value) {
	return A2(_elm_lang$html$Html_Attributes$attribute, 'manifest', value);
};
var _elm_lang$html$Html_Attributes$property = _elm_lang$virtual_dom$VirtualDom$property;
var _elm_lang$html$Html_Attributes$stringProperty = F2(
	function (name, string) {
		return A2(
			_elm_lang$html$Html_Attributes$property,
			name,
			_elm_lang$core$Json_Encode$string(string));
	});
var _elm_lang$html$Html_Attributes$class = function (name) {
	return A2(_elm_lang$html$Html_Attributes$stringProperty, 'className', name);
};
var _elm_lang$html$Html_Attributes$id = function (name) {
	return A2(_elm_lang$html$Html_Attributes$stringProperty, 'id', name);
};
var _elm_lang$html$Html_Attributes$title = function (name) {
	return A2(_elm_lang$html$Html_Attributes$stringProperty, 'title', name);
};
var _elm_lang$html$Html_Attributes$accesskey = function ($char) {
	return A2(
		_elm_lang$html$Html_Attributes$stringProperty,
		'accessKey',
		_elm_lang$core$String$fromChar($char));
};
var _elm_lang$html$Html_Attributes$dir = function (value) {
	return A2(_elm_lang$html$Html_Attributes$stringProperty, 'dir', value);
};
var _elm_lang$html$Html_Attributes$dropzone = function (value) {
	return A2(_elm_lang$html$Html_Attributes$stringProperty, 'dropzone', value);
};
var _elm_lang$html$Html_Attributes$lang = function (value) {
	return A2(_elm_lang$html$Html_Attributes$stringProperty, 'lang', value);
};
var _elm_lang$html$Html_Attributes$content = function (value) {
	return A2(_elm_lang$html$Html_Attributes$stringProperty, 'content', value);
};
var _elm_lang$html$Html_Attributes$httpEquiv = function (value) {
	return A2(_elm_lang$html$Html_Attributes$stringProperty, 'httpEquiv', value);
};
var _elm_lang$html$Html_Attributes$language = function (value) {
	return A2(_elm_lang$html$Html_Attributes$stringProperty, 'language', value);
};
var _elm_lang$html$Html_Attributes$src = function (value) {
	return A2(_elm_lang$html$Html_Attributes$stringProperty, 'src', value);
};
var _elm_lang$html$Html_Attributes$alt = function (value) {
	return A2(_elm_lang$html$Html_Attributes$stringProperty, 'alt', value);
};
var _elm_lang$html$Html_Attributes$preload = function (value) {
	return A2(_elm_lang$html$Html_Attributes$stringProperty, 'preload', value);
};
var _elm_lang$html$Html_Attributes$poster = function (value) {
	return A2(_elm_lang$html$Html_Attributes$stringProperty, 'poster', value);
};
var _elm_lang$html$Html_Attributes$kind = function (value) {
	return A2(_elm_lang$html$Html_Attributes$stringProperty, 'kind', value);
};
var _elm_lang$html$Html_Attributes$srclang = function (value) {
	return A2(_elm_lang$html$Html_Attributes$stringProperty, 'srclang', value);
};
var _elm_lang$html$Html_Attributes$sandbox = function (value) {
	return A2(_elm_lang$html$Html_Attributes$stringProperty, 'sandbox', value);
};
var _elm_lang$html$Html_Attributes$srcdoc = function (value) {
	return A2(_elm_lang$html$Html_Attributes$stringProperty, 'srcdoc', value);
};
var _elm_lang$html$Html_Attributes$type_ = function (value) {
	return A2(_elm_lang$html$Html_Attributes$stringProperty, 'type', value);
};
var _elm_lang$html$Html_Attributes$value = function (value) {
	return A2(_elm_lang$html$Html_Attributes$stringProperty, 'value', value);
};
var _elm_lang$html$Html_Attributes$defaultValue = function (value) {
	return A2(_elm_lang$html$Html_Attributes$stringProperty, 'defaultValue', value);
};
var _elm_lang$html$Html_Attributes$placeholder = function (value) {
	return A2(_elm_lang$html$Html_Attributes$stringProperty, 'placeholder', value);
};
var _elm_lang$html$Html_Attributes$accept = function (value) {
	return A2(_elm_lang$html$Html_Attributes$stringProperty, 'accept', value);
};
var _elm_lang$html$Html_Attributes$acceptCharset = function (value) {
	return A2(_elm_lang$html$Html_Attributes$stringProperty, 'acceptCharset', value);
};
var _elm_lang$html$Html_Attributes$action = function (value) {
	return A2(_elm_lang$html$Html_Attributes$stringProperty, 'action', value);
};
var _elm_lang$html$Html_Attributes$autocomplete = function (bool) {
	return A2(
		_elm_lang$html$Html_Attributes$stringProperty,
		'autocomplete',
		bool ? 'on' : 'off');
};
var _elm_lang$html$Html_Attributes$enctype = function (value) {
	return A2(_elm_lang$html$Html_Attributes$stringProperty, 'enctype', value);
};
var _elm_lang$html$Html_Attributes$method = function (value) {
	return A2(_elm_lang$html$Html_Attributes$stringProperty, 'method', value);
};
var _elm_lang$html$Html_Attributes$name = function (value) {
	return A2(_elm_lang$html$Html_Attributes$stringProperty, 'name', value);
};
var _elm_lang$html$Html_Attributes$pattern = function (value) {
	return A2(_elm_lang$html$Html_Attributes$stringProperty, 'pattern', value);
};
var _elm_lang$html$Html_Attributes$for = function (value) {
	return A2(_elm_lang$html$Html_Attributes$stringProperty, 'htmlFor', value);
};
var _elm_lang$html$Html_Attributes$max = function (value) {
	return A2(_elm_lang$html$Html_Attributes$stringProperty, 'max', value);
};
var _elm_lang$html$Html_Attributes$min = function (value) {
	return A2(_elm_lang$html$Html_Attributes$stringProperty, 'min', value);
};
var _elm_lang$html$Html_Attributes$step = function (n) {
	return A2(_elm_lang$html$Html_Attributes$stringProperty, 'step', n);
};
var _elm_lang$html$Html_Attributes$wrap = function (value) {
	return A2(_elm_lang$html$Html_Attributes$stringProperty, 'wrap', value);
};
var _elm_lang$html$Html_Attributes$usemap = function (value) {
	return A2(_elm_lang$html$Html_Attributes$stringProperty, 'useMap', value);
};
var _elm_lang$html$Html_Attributes$shape = function (value) {
	return A2(_elm_lang$html$Html_Attributes$stringProperty, 'shape', value);
};
var _elm_lang$html$Html_Attributes$coords = function (value) {
	return A2(_elm_lang$html$Html_Attributes$stringProperty, 'coords', value);
};
var _elm_lang$html$Html_Attributes$keytype = function (value) {
	return A2(_elm_lang$html$Html_Attributes$stringProperty, 'keytype', value);
};
var _elm_lang$html$Html_Attributes$align = function (value) {
	return A2(_elm_lang$html$Html_Attributes$stringProperty, 'align', value);
};
var _elm_lang$html$Html_Attributes$cite = function (value) {
	return A2(_elm_lang$html$Html_Attributes$stringProperty, 'cite', value);
};
var _elm_lang$html$Html_Attributes$href = function (value) {
	return A2(_elm_lang$html$Html_Attributes$stringProperty, 'href', value);
};
var _elm_lang$html$Html_Attributes$target = function (value) {
	return A2(_elm_lang$html$Html_Attributes$stringProperty, 'target', value);
};
var _elm_lang$html$Html_Attributes$downloadAs = function (value) {
	return A2(_elm_lang$html$Html_Attributes$stringProperty, 'download', value);
};
var _elm_lang$html$Html_Attributes$hreflang = function (value) {
	return A2(_elm_lang$html$Html_Attributes$stringProperty, 'hreflang', value);
};
var _elm_lang$html$Html_Attributes$ping = function (value) {
	return A2(_elm_lang$html$Html_Attributes$stringProperty, 'ping', value);
};
var _elm_lang$html$Html_Attributes$start = function (n) {
	return A2(
		_elm_lang$html$Html_Attributes$stringProperty,
		'start',
		_elm_lang$core$Basics$toString(n));
};
var _elm_lang$html$Html_Attributes$headers = function (value) {
	return A2(_elm_lang$html$Html_Attributes$stringProperty, 'headers', value);
};
var _elm_lang$html$Html_Attributes$scope = function (value) {
	return A2(_elm_lang$html$Html_Attributes$stringProperty, 'scope', value);
};
var _elm_lang$html$Html_Attributes$boolProperty = F2(
	function (name, bool) {
		return A2(
			_elm_lang$html$Html_Attributes$property,
			name,
			_elm_lang$core$Json_Encode$bool(bool));
	});
var _elm_lang$html$Html_Attributes$hidden = function (bool) {
	return A2(_elm_lang$html$Html_Attributes$boolProperty, 'hidden', bool);
};
var _elm_lang$html$Html_Attributes$contenteditable = function (bool) {
	return A2(_elm_lang$html$Html_Attributes$boolProperty, 'contentEditable', bool);
};
var _elm_lang$html$Html_Attributes$spellcheck = function (bool) {
	return A2(_elm_lang$html$Html_Attributes$boolProperty, 'spellcheck', bool);
};
var _elm_lang$html$Html_Attributes$async = function (bool) {
	return A2(_elm_lang$html$Html_Attributes$boolProperty, 'async', bool);
};
var _elm_lang$html$Html_Attributes$defer = function (bool) {
	return A2(_elm_lang$html$Html_Attributes$boolProperty, 'defer', bool);
};
var _elm_lang$html$Html_Attributes$scoped = function (bool) {
	return A2(_elm_lang$html$Html_Attributes$boolProperty, 'scoped', bool);
};
var _elm_lang$html$Html_Attributes$autoplay = function (bool) {
	return A2(_elm_lang$html$Html_Attributes$boolProperty, 'autoplay', bool);
};
var _elm_lang$html$Html_Attributes$controls = function (bool) {
	return A2(_elm_lang$html$Html_Attributes$boolProperty, 'controls', bool);
};
var _elm_lang$html$Html_Attributes$loop = function (bool) {
	return A2(_elm_lang$html$Html_Attributes$boolProperty, 'loop', bool);
};
var _elm_lang$html$Html_Attributes$default = function (bool) {
	return A2(_elm_lang$html$Html_Attributes$boolProperty, 'default', bool);
};
var _elm_lang$html$Html_Attributes$seamless = function (bool) {
	return A2(_elm_lang$html$Html_Attributes$boolProperty, 'seamless', bool);
};
var _elm_lang$html$Html_Attributes$checked = function (bool) {
	return A2(_elm_lang$html$Html_Attributes$boolProperty, 'checked', bool);
};
var _elm_lang$html$Html_Attributes$selected = function (bool) {
	return A2(_elm_lang$html$Html_Attributes$boolProperty, 'selected', bool);
};
var _elm_lang$html$Html_Attributes$autofocus = function (bool) {
	return A2(_elm_lang$html$Html_Attributes$boolProperty, 'autofocus', bool);
};
var _elm_lang$html$Html_Attributes$disabled = function (bool) {
	return A2(_elm_lang$html$Html_Attributes$boolProperty, 'disabled', bool);
};
var _elm_lang$html$Html_Attributes$multiple = function (bool) {
	return A2(_elm_lang$html$Html_Attributes$boolProperty, 'multiple', bool);
};
var _elm_lang$html$Html_Attributes$novalidate = function (bool) {
	return A2(_elm_lang$html$Html_Attributes$boolProperty, 'noValidate', bool);
};
var _elm_lang$html$Html_Attributes$readonly = function (bool) {
	return A2(_elm_lang$html$Html_Attributes$boolProperty, 'readOnly', bool);
};
var _elm_lang$html$Html_Attributes$required = function (bool) {
	return A2(_elm_lang$html$Html_Attributes$boolProperty, 'required', bool);
};
var _elm_lang$html$Html_Attributes$ismap = function (value) {
	return A2(_elm_lang$html$Html_Attributes$boolProperty, 'isMap', value);
};
var _elm_lang$html$Html_Attributes$download = function (bool) {
	return A2(_elm_lang$html$Html_Attributes$boolProperty, 'download', bool);
};
var _elm_lang$html$Html_Attributes$reversed = function (bool) {
	return A2(_elm_lang$html$Html_Attributes$boolProperty, 'reversed', bool);
};
var _elm_lang$html$Html_Attributes$classList = function (list) {
	return _elm_lang$html$Html_Attributes$class(
		A2(
			_elm_lang$core$String$join,
			' ',
			A2(
				_elm_lang$core$List$map,
				_elm_lang$core$Tuple$first,
				A2(_elm_lang$core$List$filter, _elm_lang$core$Tuple$second, list))));
};
var _elm_lang$html$Html_Attributes$style = _elm_lang$virtual_dom$VirtualDom$style;

var _elm_lang$html$Html_Events$keyCode = A2(_elm_lang$core$Json_Decode$field, 'keyCode', _elm_lang$core$Json_Decode$int);
var _elm_lang$html$Html_Events$targetChecked = A2(
	_elm_lang$core$Json_Decode$at,
	{
		ctor: '::',
		_0: 'target',
		_1: {
			ctor: '::',
			_0: 'checked',
			_1: {ctor: '[]'}
		}
	},
	_elm_lang$core$Json_Decode$bool);
var _elm_lang$html$Html_Events$targetValue = A2(
	_elm_lang$core$Json_Decode$at,
	{
		ctor: '::',
		_0: 'target',
		_1: {
			ctor: '::',
			_0: 'value',
			_1: {ctor: '[]'}
		}
	},
	_elm_lang$core$Json_Decode$string);
var _elm_lang$html$Html_Events$defaultOptions = _elm_lang$virtual_dom$VirtualDom$defaultOptions;
var _elm_lang$html$Html_Events$onWithOptions = _elm_lang$virtual_dom$VirtualDom$onWithOptions;
var _elm_lang$html$Html_Events$on = _elm_lang$virtual_dom$VirtualDom$on;
var _elm_lang$html$Html_Events$onFocus = function (msg) {
	return A2(
		_elm_lang$html$Html_Events$on,
		'focus',
		_elm_lang$core$Json_Decode$succeed(msg));
};
var _elm_lang$html$Html_Events$onBlur = function (msg) {
	return A2(
		_elm_lang$html$Html_Events$on,
		'blur',
		_elm_lang$core$Json_Decode$succeed(msg));
};
var _elm_lang$html$Html_Events$onSubmitOptions = _elm_lang$core$Native_Utils.update(
	_elm_lang$html$Html_Events$defaultOptions,
	{preventDefault: true});
var _elm_lang$html$Html_Events$onSubmit = function (msg) {
	return A3(
		_elm_lang$html$Html_Events$onWithOptions,
		'submit',
		_elm_lang$html$Html_Events$onSubmitOptions,
		_elm_lang$core$Json_Decode$succeed(msg));
};
var _elm_lang$html$Html_Events$onCheck = function (tagger) {
	return A2(
		_elm_lang$html$Html_Events$on,
		'change',
		A2(_elm_lang$core$Json_Decode$map, tagger, _elm_lang$html$Html_Events$targetChecked));
};
var _elm_lang$html$Html_Events$onInput = function (tagger) {
	return A2(
		_elm_lang$html$Html_Events$on,
		'input',
		A2(_elm_lang$core$Json_Decode$map, tagger, _elm_lang$html$Html_Events$targetValue));
};
var _elm_lang$html$Html_Events$onMouseOut = function (msg) {
	return A2(
		_elm_lang$html$Html_Events$on,
		'mouseout',
		_elm_lang$core$Json_Decode$succeed(msg));
};
var _elm_lang$html$Html_Events$onMouseOver = function (msg) {
	return A2(
		_elm_lang$html$Html_Events$on,
		'mouseover',
		_elm_lang$core$Json_Decode$succeed(msg));
};
var _elm_lang$html$Html_Events$onMouseLeave = function (msg) {
	return A2(
		_elm_lang$html$Html_Events$on,
		'mouseleave',
		_elm_lang$core$Json_Decode$succeed(msg));
};
var _elm_lang$html$Html_Events$onMouseEnter = function (msg) {
	return A2(
		_elm_lang$html$Html_Events$on,
		'mouseenter',
		_elm_lang$core$Json_Decode$succeed(msg));
};
var _elm_lang$html$Html_Events$onMouseUp = function (msg) {
	return A2(
		_elm_lang$html$Html_Events$on,
		'mouseup',
		_elm_lang$core$Json_Decode$succeed(msg));
};
var _elm_lang$html$Html_Events$onMouseDown = function (msg) {
	return A2(
		_elm_lang$html$Html_Events$on,
		'mousedown',
		_elm_lang$core$Json_Decode$succeed(msg));
};
var _elm_lang$html$Html_Events$onDoubleClick = function (msg) {
	return A2(
		_elm_lang$html$Html_Events$on,
		'dblclick',
		_elm_lang$core$Json_Decode$succeed(msg));
};
var _elm_lang$html$Html_Events$onClick = function (msg) {
	return A2(
		_elm_lang$html$Html_Events$on,
		'click',
		_elm_lang$core$Json_Decode$succeed(msg));
};
var _elm_lang$html$Html_Events$Options = F2(
	function (a, b) {
		return {stopPropagation: a, preventDefault: b};
	});

var _elm_lang$http$Native_Http = function() {


// ENCODING AND DECODING

function encodeUri(string)
{
	return encodeURIComponent(string);
}

function decodeUri(string)
{
	try
	{
		return _elm_lang$core$Maybe$Just(decodeURIComponent(string));
	}
	catch(e)
	{
		return _elm_lang$core$Maybe$Nothing;
	}
}


// SEND REQUEST

function toTask(request, maybeProgress)
{
	return _elm_lang$core$Native_Scheduler.nativeBinding(function(callback)
	{
		var xhr = new XMLHttpRequest();

		configureProgress(xhr, maybeProgress);

		xhr.addEventListener('error', function() {
			callback(_elm_lang$core$Native_Scheduler.fail({ ctor: 'NetworkError' }));
		});
		xhr.addEventListener('timeout', function() {
			callback(_elm_lang$core$Native_Scheduler.fail({ ctor: 'Timeout' }));
		});
		xhr.addEventListener('load', function() {
			callback(handleResponse(xhr, request.expect.responseToResult));
		});

		try
		{
			xhr.open(request.method, request.url, true);
		}
		catch (e)
		{
			return callback(_elm_lang$core$Native_Scheduler.fail({ ctor: 'BadUrl', _0: request.url }));
		}

		configureRequest(xhr, request);
		send(xhr, request.body);

		return function() { xhr.abort(); };
	});
}

function configureProgress(xhr, maybeProgress)
{
	if (maybeProgress.ctor === 'Nothing')
	{
		return;
	}

	xhr.addEventListener('progress', function(event) {
		if (!event.lengthComputable)
		{
			return;
		}
		_elm_lang$core$Native_Scheduler.rawSpawn(maybeProgress._0({
			bytes: event.loaded,
			bytesExpected: event.total
		}));
	});
}

function configureRequest(xhr, request)
{
	function setHeader(pair)
	{
		xhr.setRequestHeader(pair._0, pair._1);
	}

	A2(_elm_lang$core$List$map, setHeader, request.headers);
	xhr.responseType = request.expect.responseType;
	xhr.withCredentials = request.withCredentials;

	if (request.timeout.ctor === 'Just')
	{
		xhr.timeout = request.timeout._0;
	}
}

function send(xhr, body)
{
	switch (body.ctor)
	{
		case 'EmptyBody':
			xhr.send();
			return;

		case 'StringBody':
			xhr.setRequestHeader('Content-Type', body._0);
			xhr.send(body._1);
			return;

		case 'FormDataBody':
			xhr.send(body._0);
			return;
	}
}


// RESPONSES

function handleResponse(xhr, responseToResult)
{
	var response = toResponse(xhr);

	if (xhr.status < 200 || 300 <= xhr.status)
	{
		response.body = xhr.responseText;
		return _elm_lang$core$Native_Scheduler.fail({
			ctor: 'BadStatus',
			_0: response
		});
	}

	var result = responseToResult(response);

	if (result.ctor === 'Ok')
	{
		return _elm_lang$core$Native_Scheduler.succeed(result._0);
	}
	else
	{
		response.body = xhr.responseText;
		return _elm_lang$core$Native_Scheduler.fail({
			ctor: 'BadPayload',
			_0: result._0,
			_1: response
		});
	}
}

function toResponse(xhr)
{
	return {
		status: { code: xhr.status, message: xhr.statusText },
		headers: parseHeaders(xhr.getAllResponseHeaders()),
		url: xhr.responseURL,
		body: xhr.response
	};
}

function parseHeaders(rawHeaders)
{
	var headers = _elm_lang$core$Dict$empty;

	if (!rawHeaders)
	{
		return headers;
	}

	var headerPairs = rawHeaders.split('\u000d\u000a');
	for (var i = headerPairs.length; i--; )
	{
		var headerPair = headerPairs[i];
		var index = headerPair.indexOf('\u003a\u0020');
		if (index > 0)
		{
			var key = headerPair.substring(0, index);
			var value = headerPair.substring(index + 2);

			headers = A3(_elm_lang$core$Dict$update, key, function(oldValue) {
				if (oldValue.ctor === 'Just')
				{
					return _elm_lang$core$Maybe$Just(value + ', ' + oldValue._0);
				}
				return _elm_lang$core$Maybe$Just(value);
			}, headers);
		}
	}

	return headers;
}


// EXPECTORS

function expectStringResponse(responseToResult)
{
	return {
		responseType: 'text',
		responseToResult: responseToResult
	};
}

function mapExpect(func, expect)
{
	return {
		responseType: expect.responseType,
		responseToResult: function(response) {
			var convertedResponse = expect.responseToResult(response);
			return A2(_elm_lang$core$Result$map, func, convertedResponse);
		}
	};
}


// BODY

function multipart(parts)
{
	var formData = new FormData();

	while (parts.ctor !== '[]')
	{
		var part = parts._0;
		formData.append(part._0, part._1);
		parts = parts._1;
	}

	return { ctor: 'FormDataBody', _0: formData };
}

return {
	toTask: F2(toTask),
	expectStringResponse: expectStringResponse,
	mapExpect: F2(mapExpect),
	multipart: multipart,
	encodeUri: encodeUri,
	decodeUri: decodeUri
};

}();

var _elm_lang$http$Http_Internal$map = F2(
	function (func, request) {
		return _elm_lang$core$Native_Utils.update(
			request,
			{
				expect: A2(_elm_lang$http$Native_Http.mapExpect, func, request.expect)
			});
	});
var _elm_lang$http$Http_Internal$RawRequest = F7(
	function (a, b, c, d, e, f, g) {
		return {method: a, headers: b, url: c, body: d, expect: e, timeout: f, withCredentials: g};
	});
var _elm_lang$http$Http_Internal$Request = function (a) {
	return {ctor: 'Request', _0: a};
};
var _elm_lang$http$Http_Internal$Expect = {ctor: 'Expect'};
var _elm_lang$http$Http_Internal$FormDataBody = {ctor: 'FormDataBody'};
var _elm_lang$http$Http_Internal$StringBody = F2(
	function (a, b) {
		return {ctor: 'StringBody', _0: a, _1: b};
	});
var _elm_lang$http$Http_Internal$EmptyBody = {ctor: 'EmptyBody'};
var _elm_lang$http$Http_Internal$Header = F2(
	function (a, b) {
		return {ctor: 'Header', _0: a, _1: b};
	});

var _elm_lang$http$Http$decodeUri = _elm_lang$http$Native_Http.decodeUri;
var _elm_lang$http$Http$encodeUri = _elm_lang$http$Native_Http.encodeUri;
var _elm_lang$http$Http$expectStringResponse = _elm_lang$http$Native_Http.expectStringResponse;
var _elm_lang$http$Http$expectJson = function (decoder) {
	return _elm_lang$http$Http$expectStringResponse(
		function (response) {
			return A2(_elm_lang$core$Json_Decode$decodeString, decoder, response.body);
		});
};
var _elm_lang$http$Http$expectString = _elm_lang$http$Http$expectStringResponse(
	function (response) {
		return _elm_lang$core$Result$Ok(response.body);
	});
var _elm_lang$http$Http$multipartBody = _elm_lang$http$Native_Http.multipart;
var _elm_lang$http$Http$stringBody = _elm_lang$http$Http_Internal$StringBody;
var _elm_lang$http$Http$jsonBody = function (value) {
	return A2(
		_elm_lang$http$Http_Internal$StringBody,
		'application/json',
		A2(_elm_lang$core$Json_Encode$encode, 0, value));
};
var _elm_lang$http$Http$emptyBody = _elm_lang$http$Http_Internal$EmptyBody;
var _elm_lang$http$Http$header = _elm_lang$http$Http_Internal$Header;
var _elm_lang$http$Http$request = _elm_lang$http$Http_Internal$Request;
var _elm_lang$http$Http$post = F3(
	function (url, body, decoder) {
		return _elm_lang$http$Http$request(
			{
				method: 'POST',
				headers: {ctor: '[]'},
				url: url,
				body: body,
				expect: _elm_lang$http$Http$expectJson(decoder),
				timeout: _elm_lang$core$Maybe$Nothing,
				withCredentials: false
			});
	});
var _elm_lang$http$Http$get = F2(
	function (url, decoder) {
		return _elm_lang$http$Http$request(
			{
				method: 'GET',
				headers: {ctor: '[]'},
				url: url,
				body: _elm_lang$http$Http$emptyBody,
				expect: _elm_lang$http$Http$expectJson(decoder),
				timeout: _elm_lang$core$Maybe$Nothing,
				withCredentials: false
			});
	});
var _elm_lang$http$Http$getString = function (url) {
	return _elm_lang$http$Http$request(
		{
			method: 'GET',
			headers: {ctor: '[]'},
			url: url,
			body: _elm_lang$http$Http$emptyBody,
			expect: _elm_lang$http$Http$expectString,
			timeout: _elm_lang$core$Maybe$Nothing,
			withCredentials: false
		});
};
var _elm_lang$http$Http$toTask = function (_p0) {
	var _p1 = _p0;
	return A2(_elm_lang$http$Native_Http.toTask, _p1._0, _elm_lang$core$Maybe$Nothing);
};
var _elm_lang$http$Http$send = F2(
	function (resultToMessage, request) {
		return A2(
			_elm_lang$core$Task$attempt,
			resultToMessage,
			_elm_lang$http$Http$toTask(request));
	});
var _elm_lang$http$Http$Response = F4(
	function (a, b, c, d) {
		return {url: a, status: b, headers: c, body: d};
	});
var _elm_lang$http$Http$BadPayload = F2(
	function (a, b) {
		return {ctor: 'BadPayload', _0: a, _1: b};
	});
var _elm_lang$http$Http$BadStatus = function (a) {
	return {ctor: 'BadStatus', _0: a};
};
var _elm_lang$http$Http$NetworkError = {ctor: 'NetworkError'};
var _elm_lang$http$Http$Timeout = {ctor: 'Timeout'};
var _elm_lang$http$Http$BadUrl = function (a) {
	return {ctor: 'BadUrl', _0: a};
};
var _elm_lang$http$Http$StringPart = F2(
	function (a, b) {
		return {ctor: 'StringPart', _0: a, _1: b};
	});
var _elm_lang$http$Http$stringPart = _elm_lang$http$Http$StringPart;

var _krisajenkins$elm_exts$Exts_Html_Bootstrap$video = F2(
	function (ratio, url) {
		var ratioClass = function () {
			var _p0 = ratio;
			if (_p0.ctor === 'SixteenByNine') {
				return 'embed-responsive-16by9';
			} else {
				return 'embed-responsive-4by3';
			}
		}();
		return A2(
			_elm_lang$html$Html$div,
			{ctor: '[]'},
			{
				ctor: '::',
				_0: A2(
					_elm_lang$html$Html$h1,
					{ctor: '[]'},
					{
						ctor: '::',
						_0: _elm_lang$html$Html$text('About'),
						_1: {ctor: '[]'}
					}),
				_1: {
					ctor: '::',
					_0: A2(
						_elm_lang$html$Html$div,
						{
							ctor: '::',
							_0: _elm_lang$html$Html_Attributes$class('embed-responsive'),
							_1: {ctor: '[]'}
						},
						{
							ctor: '::',
							_0: A2(
								_elm_lang$html$Html$iframe,
								{
									ctor: '::',
									_0: _elm_lang$html$Html_Attributes$class('embed-responsive-item'),
									_1: {
										ctor: '::',
										_0: _elm_lang$html$Html_Attributes$src(url),
										_1: {ctor: '[]'}
									}
								},
								{ctor: '[]'}),
							_1: {ctor: '[]'}
						}),
					_1: {ctor: '[]'}
				}
			});
	});
var _krisajenkins$elm_exts$Exts_Html_Bootstrap$badge = _elm_lang$html$Html$span(
	{
		ctor: '::',
		_0: _elm_lang$html$Html_Attributes$class('badge'),
		_1: {ctor: '[]'}
	});
var _krisajenkins$elm_exts$Exts_Html_Bootstrap$well = _elm_lang$html$Html$div(
	{
		ctor: '::',
		_0: _elm_lang$html$Html_Attributes$class('well'),
		_1: {ctor: '[]'}
	});
var _krisajenkins$elm_exts$Exts_Html_Bootstrap$jumbotron = _elm_lang$html$Html$div(
	{
		ctor: '::',
		_0: _elm_lang$html$Html_Attributes$class('jumbotron'),
		_1: {ctor: '[]'}
	});
var _krisajenkins$elm_exts$Exts_Html_Bootstrap$clearfix = A2(
	_elm_lang$html$Html$div,
	{
		ctor: '::',
		_0: _elm_lang$html$Html_Attributes$class('clearfix'),
		_1: {ctor: '[]'}
	},
	{ctor: '[]'});
var _krisajenkins$elm_exts$Exts_Html_Bootstrap$empty = A2(
	_elm_lang$html$Html$span,
	{ctor: '[]'},
	{ctor: '[]'});
var _krisajenkins$elm_exts$Exts_Html_Bootstrap$formGroup = _elm_lang$html$Html$div(
	{
		ctor: '::',
		_0: _elm_lang$html$Html_Attributes$class('form-group'),
		_1: {ctor: '[]'}
	});
var _krisajenkins$elm_exts$Exts_Html_Bootstrap$row = _elm_lang$html$Html$div(
	{
		ctor: '::',
		_0: _elm_lang$html$Html_Attributes$class('row'),
		_1: {ctor: '[]'}
	});
var _krisajenkins$elm_exts$Exts_Html_Bootstrap$twoColumns = F2(
	function (left, right) {
		return _krisajenkins$elm_exts$Exts_Html_Bootstrap$row(
			{
				ctor: '::',
				_0: A2(
					_elm_lang$html$Html$div,
					{
						ctor: '::',
						_0: _elm_lang$html$Html_Attributes$class('col-xs-6'),
						_1: {ctor: '[]'}
					},
					left),
				_1: {
					ctor: '::',
					_0: A2(
						_elm_lang$html$Html$div,
						{
							ctor: '::',
							_0: _elm_lang$html$Html_Attributes$class('col-xs-6'),
							_1: {ctor: '[]'}
						},
						right),
					_1: {ctor: '[]'}
				}
			});
	});
var _krisajenkins$elm_exts$Exts_Html_Bootstrap$containerFluid = _elm_lang$html$Html$div(
	{
		ctor: '::',
		_0: _elm_lang$html$Html_Attributes$class('container-fluid'),
		_1: {ctor: '[]'}
	});
var _krisajenkins$elm_exts$Exts_Html_Bootstrap$container = _elm_lang$html$Html$div(
	{
		ctor: '::',
		_0: _elm_lang$html$Html_Attributes$class('container'),
		_1: {ctor: '[]'}
	});
var _krisajenkins$elm_exts$Exts_Html_Bootstrap$stylesheet = A3(
	_elm_lang$html$Html$node,
	'link',
	{
		ctor: '::',
		_0: _elm_lang$html$Html_Attributes$rel('stylesheet'),
		_1: {
			ctor: '::',
			_0: _elm_lang$html$Html_Attributes$href('https://maxcdn.bootstrapcdn.com/bootstrap/3.3.6/css/bootstrap.min.css'),
			_1: {ctor: '[]'}
		}
	},
	{ctor: '[]'});
var _krisajenkins$elm_exts$Exts_Html_Bootstrap$FourByThree = {ctor: 'FourByThree'};
var _krisajenkins$elm_exts$Exts_Html_Bootstrap$SixteenByNine = {ctor: 'SixteenByNine'};
var _krisajenkins$elm_exts$Exts_Html_Bootstrap$Left = {ctor: 'Left'};
var _krisajenkins$elm_exts$Exts_Html_Bootstrap$Bottom = {ctor: 'Bottom'};
var _krisajenkins$elm_exts$Exts_Html_Bootstrap$Right = {ctor: 'Right'};
var _krisajenkins$elm_exts$Exts_Html_Bootstrap$Top = {ctor: 'Top'};
var _krisajenkins$elm_exts$Exts_Html_Bootstrap$popover = F5(
	function (direction, isShown, styles, title, body) {
		return A2(
			_elm_lang$html$Html$div,
			{
				ctor: '::',
				_0: _elm_lang$html$Html_Attributes$classList(
					{
						ctor: '::',
						_0: {ctor: '_Tuple2', _0: 'popover fade', _1: true},
						_1: {
							ctor: '::',
							_0: {ctor: '_Tuple2', _0: 'in', _1: isShown},
							_1: {
								ctor: '::',
								_0: {
									ctor: '_Tuple2',
									_0: 'top',
									_1: _elm_lang$core$Native_Utils.eq(direction, _krisajenkins$elm_exts$Exts_Html_Bootstrap$Top)
								},
								_1: {
									ctor: '::',
									_0: {
										ctor: '_Tuple2',
										_0: 'right',
										_1: _elm_lang$core$Native_Utils.eq(direction, _krisajenkins$elm_exts$Exts_Html_Bootstrap$Right)
									},
									_1: {
										ctor: '::',
										_0: {
											ctor: '_Tuple2',
											_0: 'bottom',
											_1: _elm_lang$core$Native_Utils.eq(direction, _krisajenkins$elm_exts$Exts_Html_Bootstrap$Bottom)
										},
										_1: {
											ctor: '::',
											_0: {
												ctor: '_Tuple2',
												_0: 'left',
												_1: _elm_lang$core$Native_Utils.eq(direction, _krisajenkins$elm_exts$Exts_Html_Bootstrap$Left)
											},
											_1: {ctor: '[]'}
										}
									}
								}
							}
						}
					}),
				_1: {
					ctor: '::',
					_0: _elm_lang$html$Html_Attributes$style(
						A2(
							_elm_lang$core$Basics_ops['++'],
							styles,
							{
								ctor: '::',
								_0: {ctor: '_Tuple2', _0: 'display', _1: 'block'},
								_1: {ctor: '[]'}
							})),
					_1: {ctor: '[]'}
				}
			},
			{
				ctor: '::',
				_0: A2(
					_elm_lang$html$Html$div,
					{
						ctor: '::',
						_0: _elm_lang$html$Html_Attributes$class('arrow'),
						_1: {ctor: '[]'}
					},
					{ctor: '[]'}),
				_1: {
					ctor: '::',
					_0: function () {
						var _p1 = title;
						if (_p1.ctor === 'Just') {
							return A2(
								_elm_lang$html$Html$h3,
								{
									ctor: '::',
									_0: _elm_lang$html$Html_Attributes$class('popover-title'),
									_1: {ctor: '[]'}
								},
								{
									ctor: '::',
									_0: _elm_lang$html$Html$text(_p1._0),
									_1: {ctor: '[]'}
								});
						} else {
							return _krisajenkins$elm_exts$Exts_Html_Bootstrap$empty;
						}
					}(),
					_1: {
						ctor: '::',
						_0: A2(
							_elm_lang$html$Html$div,
							{
								ctor: '::',
								_0: _elm_lang$html$Html_Attributes$class('popover-content'),
								_1: {ctor: '[]'}
							},
							{
								ctor: '::',
								_0: body,
								_1: {ctor: '[]'}
							}),
						_1: {ctor: '[]'}
					}
				}
			});
	});

var _krisajenkins$elm_exts$Exts_Maybe$oneOf = A2(
	_elm_lang$core$List$foldl,
	F2(
		function (x, acc) {
			return (!_elm_lang$core$Native_Utils.eq(acc, _elm_lang$core$Maybe$Nothing)) ? acc : x;
		}),
	_elm_lang$core$Maybe$Nothing);
var _krisajenkins$elm_exts$Exts_Maybe$when = F2(
	function (test, value) {
		return test ? _elm_lang$core$Maybe$Just(value) : _elm_lang$core$Maybe$Nothing;
	});
var _krisajenkins$elm_exts$Exts_Maybe$validate = F2(
	function (predicate, value) {
		return predicate(value) ? _elm_lang$core$Maybe$Just(value) : _elm_lang$core$Maybe$Nothing;
	});
var _krisajenkins$elm_exts$Exts_Maybe$matches = function (predicate) {
	return _elm_lang$core$Maybe$andThen(
		_krisajenkins$elm_exts$Exts_Maybe$validate(predicate));
};
var _krisajenkins$elm_exts$Exts_Maybe$maybeDefault = F2(
	function ($default, x) {
		var _p0 = x;
		if (_p0.ctor === 'Just') {
			return _elm_lang$core$Maybe$Just(_p0._0);
		} else {
			return _elm_lang$core$Maybe$Just($default);
		}
	});
var _krisajenkins$elm_exts$Exts_Maybe$join = F3(
	function (f, left, right) {
		var _p1 = {ctor: '_Tuple2', _0: left, _1: right};
		if (((_p1.ctor === '_Tuple2') && (_p1._0.ctor === 'Just')) && (_p1._1.ctor === 'Just')) {
			return _elm_lang$core$Maybe$Just(
				A2(f, _p1._0._0, _p1._1._0));
		} else {
			return _elm_lang$core$Maybe$Nothing;
		}
	});
var _krisajenkins$elm_exts$Exts_Maybe$catMaybes = _elm_lang$core$List$filterMap(_elm_lang$core$Basics$identity);
var _krisajenkins$elm_exts$Exts_Maybe$mappend = F2(
	function (a, b) {
		var _p2 = {ctor: '_Tuple2', _0: a, _1: b};
		if (_p2._0.ctor === 'Nothing') {
			return _elm_lang$core$Maybe$Nothing;
		} else {
			if (_p2._1.ctor === 'Nothing') {
				return _elm_lang$core$Maybe$Nothing;
			} else {
				return _elm_lang$core$Maybe$Just(
					{ctor: '_Tuple2', _0: _p2._0._0, _1: _p2._1._0});
			}
		}
	});
var _krisajenkins$elm_exts$Exts_Maybe$maybe = F2(
	function ($default, f) {
		return function (_p3) {
			return A2(
				_elm_lang$core$Maybe$withDefault,
				$default,
				A2(_elm_lang$core$Maybe$map, f, _p3));
		};
	});
var _krisajenkins$elm_exts$Exts_Maybe$isJust = function (x) {
	var _p4 = x;
	if (_p4.ctor === 'Just') {
		return true;
	} else {
		return false;
	}
};
var _krisajenkins$elm_exts$Exts_Maybe$isNothing = function (_p5) {
	return !_krisajenkins$elm_exts$Exts_Maybe$isJust(_p5);
};

var _krisajenkins$elm_dialog$Dialog$map = F2(
	function (f, config) {
		return {
			closeMessage: A2(_elm_lang$core$Maybe$map, f, config.closeMessage),
			containerClass: config.containerClass,
			header: A2(
				_elm_lang$core$Maybe$map,
				_elm_lang$html$Html$map(f),
				config.header),
			body: A2(
				_elm_lang$core$Maybe$map,
				_elm_lang$html$Html$map(f),
				config.body),
			footer: A2(
				_elm_lang$core$Maybe$map,
				_elm_lang$html$Html$map(f),
				config.footer)
		};
	});
var _krisajenkins$elm_dialog$Dialog$mapMaybe = function (_p0) {
	return _elm_lang$core$Maybe$map(
		_krisajenkins$elm_dialog$Dialog$map(_p0));
};
var _krisajenkins$elm_dialog$Dialog$backdrop = function (config) {
	return A2(
		_elm_lang$html$Html$div,
		{
			ctor: '::',
			_0: _elm_lang$html$Html_Attributes$classList(
				{
					ctor: '::',
					_0: {
						ctor: '_Tuple2',
						_0: 'modal-backdrop in',
						_1: _krisajenkins$elm_exts$Exts_Maybe$isJust(config)
					},
					_1: {ctor: '[]'}
				}),
			_1: {ctor: '[]'}
		},
		{ctor: '[]'});
};
var _krisajenkins$elm_dialog$Dialog$wrapFooter = function (footer) {
	return A2(
		_elm_lang$html$Html$div,
		{
			ctor: '::',
			_0: _elm_lang$html$Html_Attributes$class('modal-footer'),
			_1: {ctor: '[]'}
		},
		{
			ctor: '::',
			_0: footer,
			_1: {ctor: '[]'}
		});
};
var _krisajenkins$elm_dialog$Dialog$wrapBody = function (body) {
	return A2(
		_elm_lang$html$Html$div,
		{
			ctor: '::',
			_0: _elm_lang$html$Html_Attributes$class('modal-body'),
			_1: {ctor: '[]'}
		},
		{
			ctor: '::',
			_0: body,
			_1: {ctor: '[]'}
		});
};
var _krisajenkins$elm_dialog$Dialog$closeButton = function (closeMessage) {
	return A2(
		_elm_lang$html$Html$button,
		{
			ctor: '::',
			_0: _elm_lang$html$Html_Attributes$class('close'),
			_1: {
				ctor: '::',
				_0: _elm_lang$html$Html_Events$onClick(closeMessage),
				_1: {ctor: '[]'}
			}
		},
		{
			ctor: '::',
			_0: _elm_lang$html$Html$text('x'),
			_1: {ctor: '[]'}
		});
};
var _krisajenkins$elm_dialog$Dialog$wrapHeader = F2(
	function (closeMessage, header) {
		return (_elm_lang$core$Native_Utils.eq(closeMessage, _elm_lang$core$Maybe$Nothing) && _elm_lang$core$Native_Utils.eq(header, _elm_lang$core$Maybe$Nothing)) ? _krisajenkins$elm_exts$Exts_Html_Bootstrap$empty : A2(
			_elm_lang$html$Html$div,
			{
				ctor: '::',
				_0: _elm_lang$html$Html_Attributes$class('modal-header'),
				_1: {ctor: '[]'}
			},
			{
				ctor: '::',
				_0: A3(_krisajenkins$elm_exts$Exts_Maybe$maybe, _krisajenkins$elm_exts$Exts_Html_Bootstrap$empty, _krisajenkins$elm_dialog$Dialog$closeButton, closeMessage),
				_1: {
					ctor: '::',
					_0: A2(_elm_lang$core$Maybe$withDefault, _krisajenkins$elm_exts$Exts_Html_Bootstrap$empty, header),
					_1: {ctor: '[]'}
				}
			});
	});
var _krisajenkins$elm_dialog$Dialog$view = function (maybeConfig) {
	var displayed = _krisajenkins$elm_exts$Exts_Maybe$isJust(maybeConfig);
	return A2(
		_elm_lang$html$Html$div,
		function () {
			var _p1 = A2(
				_elm_lang$core$Maybe$andThen,
				function (_) {
					return _.containerClass;
				},
				maybeConfig);
			if (_p1.ctor === 'Nothing') {
				return {ctor: '[]'};
			} else {
				return {
					ctor: '::',
					_0: _elm_lang$html$Html_Attributes$class(_p1._0),
					_1: {ctor: '[]'}
				};
			}
		}(),
		{
			ctor: '::',
			_0: A2(
				_elm_lang$html$Html$div,
				{
					ctor: '::',
					_0: _elm_lang$html$Html_Attributes$classList(
						{
							ctor: '::',
							_0: {ctor: '_Tuple2', _0: 'modal', _1: true},
							_1: {
								ctor: '::',
								_0: {ctor: '_Tuple2', _0: 'in', _1: displayed},
								_1: {ctor: '[]'}
							}
						}),
					_1: {
						ctor: '::',
						_0: _elm_lang$html$Html_Attributes$style(
							{
								ctor: '::',
								_0: {
									ctor: '_Tuple2',
									_0: 'display',
									_1: displayed ? 'block' : 'none'
								},
								_1: {ctor: '[]'}
							}),
						_1: {ctor: '[]'}
					}
				},
				{
					ctor: '::',
					_0: A2(
						_elm_lang$html$Html$div,
						{
							ctor: '::',
							_0: _elm_lang$html$Html_Attributes$class('modal-dialog'),
							_1: {ctor: '[]'}
						},
						{
							ctor: '::',
							_0: A2(
								_elm_lang$html$Html$div,
								{
									ctor: '::',
									_0: _elm_lang$html$Html_Attributes$class('modal-content'),
									_1: {ctor: '[]'}
								},
								function () {
									var _p2 = maybeConfig;
									if (_p2.ctor === 'Nothing') {
										return {
											ctor: '::',
											_0: _krisajenkins$elm_exts$Exts_Html_Bootstrap$empty,
											_1: {ctor: '[]'}
										};
									} else {
										var _p3 = _p2._0;
										return {
											ctor: '::',
											_0: A2(_krisajenkins$elm_dialog$Dialog$wrapHeader, _p3.closeMessage, _p3.header),
											_1: {
												ctor: '::',
												_0: A3(_krisajenkins$elm_exts$Exts_Maybe$maybe, _krisajenkins$elm_exts$Exts_Html_Bootstrap$empty, _krisajenkins$elm_dialog$Dialog$wrapBody, _p3.body),
												_1: {
													ctor: '::',
													_0: A3(_krisajenkins$elm_exts$Exts_Maybe$maybe, _krisajenkins$elm_exts$Exts_Html_Bootstrap$empty, _krisajenkins$elm_dialog$Dialog$wrapFooter, _p3.footer),
													_1: {ctor: '[]'}
												}
											}
										};
									}
								}()),
							_1: {ctor: '[]'}
						}),
					_1: {ctor: '[]'}
				}),
			_1: {
				ctor: '::',
				_0: _krisajenkins$elm_dialog$Dialog$backdrop(maybeConfig),
				_1: {ctor: '[]'}
			}
		});
};
var _krisajenkins$elm_dialog$Dialog$Config = F5(
	function (a, b, c, d, e) {
		return {closeMessage: a, containerClass: b, header: c, body: d, footer: e};
	});

var _user$project$Routinely$progressToReward = function (logs) {
	var totalPoints = _elm_lang$core$List$sum(
		A2(
			_elm_lang$core$List$map,
			function (_) {
				return _.value;
			},
			logs));
	return A2(_elm_lang$core$Basics$rem, totalPoints, 50) * 2;
};
var _user$project$Routinely$colorClassForPercent = function (p) {
	return (_elm_lang$core$Native_Utils.cmp(p, 33) < 1) ? 'bg-danger' : ((_elm_lang$core$Native_Utils.cmp(p, 66) < 0) ? 'bg-warning' : ((_elm_lang$core$Native_Utils.cmp(p, 90) < 0) ? 'bg-info' : 'bg-success'));
};
var _user$project$Routinely$viewRewardMeter = function (model) {
	var percentToNextReward = _user$project$Routinely$progressToReward(model.actionLogs);
	var baseClasses = 'progress-bar progress-bar-striped progress-bar-animated ';
	return A2(
		_elm_lang$html$Html$div,
		{
			ctor: '::',
			_0: _elm_lang$html$Html_Attributes$class('progress'),
			_1: {ctor: '[]'}
		},
		{
			ctor: '::',
			_0: A2(
				_elm_lang$html$Html$div,
				{
					ctor: '::',
					_0: _elm_lang$html$Html_Attributes$class(
						A2(
							_elm_lang$core$Basics_ops['++'],
							baseClasses,
							_user$project$Routinely$colorClassForPercent(percentToNextReward))),
					_1: {
						ctor: '::',
						_0: _elm_lang$html$Html_Attributes$style(
							{
								ctor: '::',
								_0: {
									ctor: '_Tuple2',
									_0: 'width',
									_1: A2(
										_elm_lang$core$Basics_ops['++'],
										_elm_lang$core$Basics$toString(percentToNextReward),
										'%')
								},
								_1: {ctor: '[]'}
							}),
						_1: {ctor: '[]'}
					}
				},
				{ctor: '[]'}),
			_1: {ctor: '[]'}
		});
};
var _user$project$Routinely$pointsTotalStr = function (logs) {
	return _elm_lang$core$Basics$toString(
		_elm_lang$core$List$sum(
			A2(
				_elm_lang$core$List$map,
				function (_) {
					return _.value;
				},
				logs)));
};
var _user$project$Routinely$viewPoints = function (logs) {
	return A2(
		_elm_lang$html$Html$div,
		{
			ctor: '::',
			_0: _elm_lang$html$Html_Attributes$class('col'),
			_1: {
				ctor: '::',
				_0: _elm_lang$html$Html_Attributes$style(
					{
						ctor: '::',
						_0: {ctor: '_Tuple2', _0: 'text-align', _1: 'right'},
						_1: {ctor: '[]'}
					}),
				_1: {ctor: '[]'}
			}
		},
		{
			ctor: '::',
			_0: A2(
				_elm_lang$html$Html$h2,
				{
					ctor: '::',
					_0: _elm_lang$html$Html_Attributes$class('align-middle'),
					_1: {ctor: '[]'}
				},
				{
					ctor: '::',
					_0: A2(
						_elm_lang$html$Html$span,
						{
							ctor: '::',
							_0: _elm_lang$html$Html_Attributes$class('badge badge-pill badge-warning'),
							_1: {ctor: '[]'}
						},
						{
							ctor: '::',
							_0: _elm_lang$html$Html$text(
								_user$project$Routinely$pointsTotalStr(logs)),
							_1: {ctor: '[]'}
						}),
					_1: {ctor: '[]'}
				}),
			_1: {ctor: '[]'}
		});
};
var _user$project$Routinely$viewIcon = function (icon) {
	return A2(
		_elm_lang$html$Html$span,
		{
			ctor: '::',
			_0: _elm_lang$html$Html_Attributes$class(
				A2(_elm_lang$core$Basics_ops['++'], 'oi oi-', icon)),
			_1: {ctor: '[]'}
		},
		{ctor: '[]'});
};
var _user$project$Routinely$viewRewardsToRedeem = function (logs) {
	var numberOfRewards = (_elm_lang$core$List$sum(
		A2(
			_elm_lang$core$List$map,
			function (_) {
				return _.value;
			},
			logs)) / 50) | 0;
	var gift = A2(
		_elm_lang$html$Html$span,
		{
			ctor: '::',
			_0: _elm_lang$html$Html_Attributes$style(
				{
					ctor: '::',
					_0: {ctor: '_Tuple2', _0: 'color', _1: 'gold'},
					_1: {
						ctor: '::',
						_0: {ctor: '_Tuple2', _0: 'font-size', _1: '2em'},
						_1: {ctor: '[]'}
					}
				}),
			_1: {ctor: '[]'}
		},
		{
			ctor: '::',
			_0: _user$project$Routinely$viewIcon('bookmark'),
			_1: {
				ctor: '::',
				_0: _elm_lang$html$Html$text(' '),
				_1: {ctor: '[]'}
			}
		});
	return A2(
		_elm_lang$html$Html$h1,
		{
			ctor: '::',
			_0: _elm_lang$html$Html_Attributes$class('text-right'),
			_1: {ctor: '[]'}
		},
		A2(_elm_lang$core$List$repeat, numberOfRewards, gift));
};
var _user$project$Routinely$classesForActionRow = F2(
	function (action, logsForAction) {
		return (_elm_lang$core$Native_Utils.cmp(
			_elm_lang$core$List$length(logsForAction),
			action.perWeek) > -1) ? 'action-week-complete' : 'action-week-incomplete';
	});
var _user$project$Routinely$weekDays = {
	ctor: '::',
	_0: _elm_community$elm_time$Time_Date$Mon,
	_1: {
		ctor: '::',
		_0: _elm_community$elm_time$Time_Date$Tue,
		_1: {
			ctor: '::',
			_0: _elm_community$elm_time$Time_Date$Wed,
			_1: {
				ctor: '::',
				_0: _elm_community$elm_time$Time_Date$Thu,
				_1: {
					ctor: '::',
					_0: _elm_community$elm_time$Time_Date$Fri,
					_1: {
						ctor: '::',
						_0: _elm_community$elm_time$Time_Date$Sat,
						_1: {
							ctor: '::',
							_0: _elm_community$elm_time$Time_Date$Sun,
							_1: {ctor: '[]'}
						}
					}
				}
			}
		}
	}
};
var _user$project$Routinely$viewTableHeaders = A2(
	_elm_lang$html$Html$tr,
	{ctor: '[]'},
	A2(
		_elm_lang$core$Basics_ops['++'],
		{
			ctor: '::',
			_0: A2(
				_elm_lang$html$Html$th,
				{ctor: '[]'},
				{
					ctor: '::',
					_0: _elm_lang$html$Html$text(''),
					_1: {ctor: '[]'}
				}),
			_1: {ctor: '[]'}
		},
		A2(
			_elm_lang$core$List$map,
			function (d) {
				return A2(
					_elm_lang$html$Html$th,
					{ctor: '[]'},
					{
						ctor: '::',
						_0: _elm_lang$html$Html$text(
							_elm_lang$core$Basics$toString(d)),
						_1: {ctor: '[]'}
					});
			},
			_user$project$Routinely$weekDays)));
var _user$project$Routinely$daysAwayFromMonday = function (time) {
	var _p0 = _elm_community$elm_time$Time_DateTime$weekday(
		_elm_community$elm_time$Time_DateTime$fromTimestamp(time));
	switch (_p0.ctor) {
		case 'Mon':
			return 0.0;
		case 'Tue':
			return 1.0;
		case 'Wed':
			return 2.0;
		case 'Thu':
			return 3.0;
		case 'Fri':
			return 4.0;
		case 'Sat':
			return 5.0;
		default:
			return 6.0;
	}
};
var _user$project$Routinely$apiPrefix = '/api';
var _user$project$Routinely$apiRoute = function (path) {
	return A2(_elm_lang$core$Basics_ops['++'], _user$project$Routinely$apiPrefix, path);
};
var _user$project$Routinely$postActionLogRequest = function (action) {
	var json = _elm_lang$core$Json_Encode$object(
		{
			ctor: '::',
			_0: {
				ctor: '_Tuple2',
				_0: 'action_id',
				_1: _elm_lang$core$Json_Encode$int(action.id)
			},
			_1: {ctor: '[]'}
		});
	var body = _elm_lang$http$Http$jsonBody(json);
	var postActionLogUrl = _user$project$Routinely$apiRoute('/rpc/create_action_log');
	return A3(_elm_lang$http$Http$post, postActionLogUrl, body, _elm_lang$core$Json_Decode$string);
};
var _user$project$Routinely$strToZonedDateTime = function (str) {
	return A2(
		_elm_community$elm_time$Time_ZonedDateTime$fromISO8601,
		_elm_community$elm_time$Time_TimeZones$us_central(
			{ctor: '_Tuple0'}),
		A2(_elm_lang$core$Basics_ops['++'], str, '+00:00'));
};
var _user$project$Routinely$logsForDay = function (day) {
	return _elm_lang$core$List$filter(
		function (log) {
			var _p1 = _user$project$Routinely$strToZonedDateTime(log.createdAt);
			if (_p1.ctor === 'Ok') {
				return _elm_lang$core$Native_Utils.eq(
					_elm_community$elm_time$Time_ZonedDateTime$weekday(_p1._0),
					day);
			} else {
				return false;
			}
		});
};
var _user$project$Routinely$classesForActionCell = F3(
	function (d, action, logs) {
		return (_elm_lang$core$Native_Utils.cmp(
			_elm_lang$core$List$length(
				A2(_user$project$Routinely$logsForDay, d, logs)),
			action.perDay) > -1) ? 'action-day-complete' : 'action-day-incomplete';
	});
var _user$project$Routinely$viewLogsForDay = F2(
	function (day, logs) {
		return A2(
			_elm_lang$core$List$map,
			function (_p2) {
				return _user$project$Routinely$viewIcon('star');
			},
			A2(_user$project$Routinely$logsForDay, day, logs));
	});
var _user$project$Routinely$viewWeekDay = F2(
	function (action, logsForAction) {
		return A2(
			_elm_lang$core$List$map,
			function (d) {
				return A2(
					_elm_lang$html$Html$td,
					{
						ctor: '::',
						_0: _elm_lang$html$Html_Attributes$class(
							A3(_user$project$Routinely$classesForActionCell, d, action, logsForAction)),
						_1: {ctor: '[]'}
					},
					A2(_user$project$Routinely$viewLogsForDay, d, logsForAction));
			},
			_user$project$Routinely$weekDays);
	});
var _user$project$Routinely$Model = F6(
	function (a, b, c, d, e, f) {
		return {actions: a, selectedAction: b, actionLogs: c, weeklyActionLogs: d, theTime: e, showDialog: f};
	});
var _user$project$Routinely$Action = F5(
	function (a, b, c, d, e) {
		return {id: a, name: b, value: c, perWeek: d, perDay: e};
	});
var _user$project$Routinely$actionDecoder = _elm_lang$core$Json_Decode$list(
	A6(
		_elm_lang$core$Json_Decode$map5,
		_user$project$Routinely$Action,
		A2(_elm_lang$core$Json_Decode$field, 'id', _elm_lang$core$Json_Decode$int),
		A2(_elm_lang$core$Json_Decode$field, 'name', _elm_lang$core$Json_Decode$string),
		A2(_elm_lang$core$Json_Decode$field, 'value', _elm_lang$core$Json_Decode$int),
		A2(_elm_lang$core$Json_Decode$field, 'per_week', _elm_lang$core$Json_Decode$int),
		A2(_elm_lang$core$Json_Decode$field, 'per_day', _elm_lang$core$Json_Decode$int)));
var _user$project$Routinely$getActionsRequest = A2(
	_elm_lang$http$Http$get,
	_user$project$Routinely$apiRoute('/actions'),
	_user$project$Routinely$actionDecoder);
var _user$project$Routinely$ActionLog = F5(
	function (a, b, c, d, e) {
		return {id: a, name: b, value: c, actionId: d, createdAt: e};
	});
var _user$project$Routinely$actionLogDecoder = _elm_lang$core$Json_Decode$list(
	A6(
		_elm_lang$core$Json_Decode$map5,
		_user$project$Routinely$ActionLog,
		A2(_elm_lang$core$Json_Decode$field, 'id', _elm_lang$core$Json_Decode$int),
		A2(_elm_lang$core$Json_Decode$field, 'name', _elm_lang$core$Json_Decode$string),
		A2(_elm_lang$core$Json_Decode$field, 'value', _elm_lang$core$Json_Decode$int),
		A2(_elm_lang$core$Json_Decode$field, 'action_id', _elm_lang$core$Json_Decode$int),
		A2(_elm_lang$core$Json_Decode$field, 'created_at', _elm_lang$core$Json_Decode$string)));
var _user$project$Routinely$actionLogsRequest = A2(
	_elm_lang$http$Http$get,
	_user$project$Routinely$apiRoute('/action_logs'),
	_user$project$Routinely$actionLogDecoder);
var _user$project$Routinely$CreateActionLogResponse = function (a) {
	return {ctor: 'CreateActionLogResponse', _0: a};
};
var _user$project$Routinely$postActionLog = function (action) {
	return A2(
		_elm_lang$http$Http$send,
		_user$project$Routinely$CreateActionLogResponse,
		_user$project$Routinely$postActionLogRequest(action));
};
var _user$project$Routinely$CreateActionLog = function (a) {
	return {ctor: 'CreateActionLog', _0: a};
};
var _user$project$Routinely$viewActionLabelCell = function (action) {
	return A2(
		_elm_lang$html$Html$td,
		{
			ctor: '::',
			_0: _elm_lang$html$Html_Events$onClick(
				_user$project$Routinely$CreateActionLog(action)),
			_1: {ctor: '[]'}
		},
		{
			ctor: '::',
			_0: _elm_lang$html$Html$text(action.name),
			_1: {ctor: '[]'}
		});
};
var _user$project$Routinely$viewWeeklyActions = F2(
	function (actions, weeklyLogs) {
		return A2(
			_elm_lang$core$List$map,
			function (action) {
				var logsForAction = A2(
					_elm_lang$core$List$filter,
					function (l) {
						return _elm_lang$core$Native_Utils.eq(l.actionId, action.id);
					},
					weeklyLogs);
				return A2(
					_elm_lang$html$Html$tr,
					{
						ctor: '::',
						_0: _elm_lang$html$Html_Attributes$class(
							A2(_user$project$Routinely$classesForActionRow, action, logsForAction)),
						_1: {ctor: '[]'}
					},
					A2(
						_elm_lang$core$Basics_ops['++'],
						{
							ctor: '::',
							_0: _user$project$Routinely$viewActionLabelCell(action),
							_1: {ctor: '[]'}
						},
						A2(_user$project$Routinely$viewWeekDay, action, logsForAction)));
			},
			actions);
	});
var _user$project$Routinely$viewActionsTable = function (model) {
	return A2(
		_elm_lang$html$Html$table,
		{
			ctor: '::',
			_0: _elm_lang$html$Html_Attributes$class('table table-bordered'),
			_1: {ctor: '[]'}
		},
		{
			ctor: '::',
			_0: A2(
				_elm_lang$html$Html$thead,
				{ctor: '[]'},
				{
					ctor: '::',
					_0: _user$project$Routinely$viewTableHeaders,
					_1: {ctor: '[]'}
				}),
			_1: {
				ctor: '::',
				_0: A2(
					_elm_lang$html$Html$tbody,
					{ctor: '[]'},
					A2(_user$project$Routinely$viewWeeklyActions, model.actions, model.weeklyActionLogs)),
				_1: {ctor: '[]'}
			}
		});
};
var _user$project$Routinely$GetActionLogsResponse = function (a) {
	return {ctor: 'GetActionLogsResponse', _0: a};
};
var _user$project$Routinely$getActionLogs = A2(_elm_lang$http$Http$send, _user$project$Routinely$GetActionLogsResponse, _user$project$Routinely$actionLogsRequest);
var _user$project$Routinely$GetActionsResponse = function (a) {
	return {ctor: 'GetActionsResponse', _0: a};
};
var _user$project$Routinely$getActions = A2(_elm_lang$http$Http$send, _user$project$Routinely$GetActionsResponse, _user$project$Routinely$getActionsRequest);
var _user$project$Routinely$CurrentTime = function (a) {
	return {ctor: 'CurrentTime', _0: a};
};
var _user$project$Routinely$init = {
	ctor: '_Tuple2',
	_0: {
		actions: {ctor: '[]'},
		selectedAction: _elm_lang$core$Maybe$Nothing,
		actionLogs: {ctor: '[]'},
		weeklyActionLogs: {ctor: '[]'},
		theTime: 0.0,
		showDialog: false
	},
	_1: A2(_elm_lang$core$Task$perform, _user$project$Routinely$CurrentTime, _elm_lang$core$Time$now)
};
var _user$project$Routinely$update = F2(
	function (msg, model) {
		var timeFromMidnight = _elm_lang$core$Basics$toFloat(
			A2(
				_elm_lang$core$Basics_ops['%'],
				_elm_lang$core$Basics$round(model.theTime),
				_elm_lang$core$Basics$round(24.0 * _elm_lang$core$Time$hour)));
		var timeFromStartOfWeek = ((_user$project$Routinely$daysAwayFromMonday(model.theTime) * 24.0) * _elm_lang$core$Time$hour) + timeFromMidnight;
		var mostRecentMonday = model.theTime - timeFromStartOfWeek;
		var _p3 = msg;
		switch (_p3.ctor) {
			case 'NoOp':
				return A2(
					_elm_lang$core$Platform_Cmd_ops['!'],
					model,
					{
						ctor: '::',
						_0: _elm_lang$core$Platform_Cmd$none,
						_1: {ctor: '[]'}
					});
			case 'CloseDialog':
				return A2(
					_elm_lang$core$Platform_Cmd_ops['!'],
					_elm_lang$core$Native_Utils.update(
						model,
						{showDialog: false, selectedAction: _elm_lang$core$Maybe$Nothing}),
					{
						ctor: '::',
						_0: _elm_lang$core$Platform_Cmd$none,
						_1: {ctor: '[]'}
					});
			case 'CurrentTime':
				return A2(
					_elm_lang$core$Platform_Cmd_ops['!'],
					_elm_lang$core$Native_Utils.update(
						model,
						{theTime: _p3._0}),
					{
						ctor: '::',
						_0: _user$project$Routinely$getActions,
						_1: {
							ctor: '::',
							_0: _user$project$Routinely$getActionLogs,
							_1: {ctor: '[]'}
						}
					});
			case 'GetActionsResponse':
				if (_p3._0.ctor === 'Ok') {
					return A2(
						_elm_lang$core$Platform_Cmd_ops['!'],
						_elm_lang$core$Native_Utils.update(
							model,
							{actions: _p3._0._0}),
						{
							ctor: '::',
							_0: _elm_lang$core$Platform_Cmd$none,
							_1: {ctor: '[]'}
						});
				} else {
					return A2(
						_elm_lang$core$Platform_Cmd_ops['!'],
						model,
						{
							ctor: '::',
							_0: _elm_lang$core$Platform_Cmd$none,
							_1: {ctor: '[]'}
						});
				}
			case 'GetActionLogsResponse':
				if (_p3._0.ctor === 'Ok') {
					var _p5 = _p3._0._0;
					return A2(
						_elm_lang$core$Platform_Cmd_ops['!'],
						_elm_lang$core$Native_Utils.update(
							model,
							{
								actionLogs: _p5,
								weeklyActionLogs: A2(
									_elm_lang$core$List$filter,
									function (l) {
										var _p4 = _user$project$Routinely$strToZonedDateTime(l.createdAt);
										if (_p4.ctor === 'Ok') {
											return _elm_lang$core$Native_Utils.cmp(
												_elm_community$elm_time$Time_ZonedDateTime$toTimestamp(_p4._0),
												mostRecentMonday) > -1;
										} else {
											return false;
										}
									},
									_p5)
							}),
						{
							ctor: '::',
							_0: _elm_lang$core$Platform_Cmd$none,
							_1: {ctor: '[]'}
						});
				} else {
					return A2(
						_elm_lang$core$Platform_Cmd_ops['!'],
						model,
						{
							ctor: '::',
							_0: _elm_lang$core$Platform_Cmd$none,
							_1: {ctor: '[]'}
						});
				}
			case 'CreateActionLog':
				return A2(
					_elm_lang$core$Platform_Cmd_ops['!'],
					_elm_lang$core$Native_Utils.update(
						model,
						{
							showDialog: true,
							selectedAction: _elm_lang$core$Maybe$Just(_p3._0)
						}),
					{
						ctor: '::',
						_0: _elm_lang$core$Platform_Cmd$none,
						_1: {ctor: '[]'}
					});
			case 'AcknowledgeCreateActionLog':
				return A2(
					_elm_lang$core$Platform_Cmd_ops['!'],
					_elm_lang$core$Native_Utils.update(
						model,
						{showDialog: false}),
					{
						ctor: '::',
						_0: _user$project$Routinely$postActionLog(_p3._0),
						_1: {ctor: '[]'}
					});
			default:
				if (_p3._0.ctor === 'Ok') {
					return A2(
						_elm_lang$core$Platform_Cmd_ops['!'],
						model,
						{
							ctor: '::',
							_0: A2(_elm_lang$core$Task$perform, _user$project$Routinely$CurrentTime, _elm_lang$core$Time$now),
							_1: {ctor: '[]'}
						});
				} else {
					return A2(
						_elm_lang$core$Platform_Cmd_ops['!'],
						model,
						{
							ctor: '::',
							_0: _elm_lang$core$Platform_Cmd$none,
							_1: {ctor: '[]'}
						});
				}
		}
	});
var _user$project$Routinely$CloseDialog = {ctor: 'CloseDialog'};
var _user$project$Routinely$AcknowledgeCreateActionLog = function (a) {
	return {ctor: 'AcknowledgeCreateActionLog', _0: a};
};
var _user$project$Routinely$actionConfirmDialogConfig = function (action) {
	return {
		body: _elm_lang$core$Maybe$Just(
			_elm_lang$html$Html$text('Are you sure?')),
		closeMessage: _elm_lang$core$Maybe$Just(_user$project$Routinely$CloseDialog),
		containerClass: _elm_lang$core$Maybe$Nothing,
		footer: _elm_lang$core$Maybe$Just(
			A2(
				_elm_lang$html$Html$button,
				{
					ctor: '::',
					_0: _elm_lang$html$Html_Attributes$class('btn btn-success'),
					_1: {
						ctor: '::',
						_0: _elm_lang$html$Html_Events$onClick(
							_user$project$Routinely$AcknowledgeCreateActionLog(action)),
						_1: {ctor: '[]'}
					}
				},
				{
					ctor: '::',
					_0: _elm_lang$html$Html$text('OK'),
					_1: {ctor: '[]'}
				})),
		header: _elm_lang$core$Maybe$Just(
			A2(
				_elm_lang$html$Html$h3,
				{ctor: '[]'},
				{
					ctor: '::',
					_0: _elm_lang$html$Html$text(action.name),
					_1: {ctor: '[]'}
				}))
	};
};
var _user$project$Routinely$view = function (model) {
	return A2(
		_elm_lang$html$Html$div,
		{
			ctor: '::',
			_0: _elm_lang$html$Html_Attributes$class('container-fluid'),
			_1: {ctor: '[]'}
		},
		{
			ctor: '::',
			_0: A2(
				_elm_lang$html$Html$div,
				{
					ctor: '::',
					_0: _elm_lang$html$Html_Attributes$class('row top'),
					_1: {ctor: '[]'}
				},
				{
					ctor: '::',
					_0: A2(
						_elm_lang$html$Html$div,
						{
							ctor: '::',
							_0: _elm_lang$html$Html_Attributes$class('col'),
							_1: {ctor: '[]'}
						},
						{
							ctor: '::',
							_0: A2(
								_elm_lang$html$Html$h1,
								{ctor: '[]'},
								{
									ctor: '::',
									_0: A2(
										_elm_lang$html$Html$span,
										{
											ctor: '::',
											_0: _elm_lang$html$Html_Attributes$class('align-middle'),
											_1: {ctor: '[]'}
										},
										{
											ctor: '::',
											_0: _elm_lang$html$Html$text('Routinely!'),
											_1: {ctor: '[]'}
										}),
									_1: {ctor: '[]'}
								}),
							_1: {ctor: '[]'}
						}),
					_1: {
						ctor: '::',
						_0: _user$project$Routinely$viewPoints(model.actionLogs),
						_1: {ctor: '[]'}
					}
				}),
			_1: {
				ctor: '::',
				_0: A2(
					_elm_lang$html$Html$div,
					{
						ctor: '::',
						_0: _elm_lang$html$Html_Attributes$class('row'),
						_1: {ctor: '[]'}
					},
					{
						ctor: '::',
						_0: A2(
							_elm_lang$html$Html$div,
							{
								ctor: '::',
								_0: _elm_lang$html$Html_Attributes$class('col'),
								_1: {ctor: '[]'}
							},
							{
								ctor: '::',
								_0: _user$project$Routinely$viewActionsTable(model),
								_1: {ctor: '[]'}
							}),
						_1: {ctor: '[]'}
					}),
				_1: {
					ctor: '::',
					_0: A2(
						_elm_lang$html$Html$div,
						{
							ctor: '::',
							_0: _elm_lang$html$Html_Attributes$class('row'),
							_1: {ctor: '[]'}
						},
						{
							ctor: '::',
							_0: A2(
								_elm_lang$html$Html$div,
								{
									ctor: '::',
									_0: _elm_lang$html$Html_Attributes$class('col'),
									_1: {ctor: '[]'}
								},
								{
									ctor: '::',
									_0: _user$project$Routinely$viewRewardMeter(model),
									_1: {ctor: '[]'}
								}),
							_1: {ctor: '[]'}
						}),
					_1: {
						ctor: '::',
						_0: A2(
							_elm_lang$html$Html$div,
							{
								ctor: '::',
								_0: _elm_lang$html$Html_Attributes$class('row'),
								_1: {ctor: '[]'}
							},
							{
								ctor: '::',
								_0: A2(
									_elm_lang$html$Html$div,
									{
										ctor: '::',
										_0: _elm_lang$html$Html_Attributes$class('col'),
										_1: {ctor: '[]'}
									},
									{
										ctor: '::',
										_0: _user$project$Routinely$viewRewardsToRedeem(model.actionLogs),
										_1: {ctor: '[]'}
									}),
								_1: {ctor: '[]'}
							}),
						_1: {
							ctor: '::',
							_0: _krisajenkins$elm_dialog$Dialog$view(
								function () {
									if (model.showDialog) {
										var _p6 = model.selectedAction;
										if (_p6.ctor === 'Just') {
											return _elm_lang$core$Maybe$Just(
												_user$project$Routinely$actionConfirmDialogConfig(_p6._0));
										} else {
											return _elm_lang$core$Maybe$Nothing;
										}
									} else {
										return _elm_lang$core$Maybe$Nothing;
									}
								}()),
							_1: {ctor: '[]'}
						}
					}
				}
			}
		});
};
var _user$project$Routinely$main = _elm_lang$html$Html$program(
	{
		init: _user$project$Routinely$init,
		update: _user$project$Routinely$update,
		subscriptions: function (_p7) {
			return _elm_lang$core$Platform_Sub$none;
		},
		view: _user$project$Routinely$view
	})();
var _user$project$Routinely$NoOp = {ctor: 'NoOp'};

var Elm = {};
Elm['Routinely'] = Elm['Routinely'] || {};
if (typeof _user$project$Routinely$main !== 'undefined') {
    _user$project$Routinely$main(Elm['Routinely'], 'Routinely', undefined);
}

if (typeof define === "function" && define['amd'])
{
  define([], function() { return Elm; });
  return;
}

if (typeof module === "object")
{
  module['exports'] = Elm;
  return;
}

var globalElm = this['Elm'];
if (typeof globalElm === "undefined")
{
  this['Elm'] = Elm;
  return;
}

for (var publicModule in Elm)
{
  if (publicModule in globalElm)
  {
    throw new Error('There are two Elm modules called `' + publicModule + '` on this page! Rename one of them.');
  }
  globalElm[publicModule] = Elm[publicModule];
}

}).call(this);

