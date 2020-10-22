local s = {}

local unpack = (_G.unpack or table.unpack)

local mt = {
	["function"] = {
		call = function(obj, ...)
			return obj(...)
		end,
	},
	number = {
		["bin<"] = function(lhs, rhs)
			return lhs < rhs
		end,
		["bin<="] = function(lhs, rhs)
			return lhs <= rhs
		end,
		["bin=="] = function(lhs, rhs)
			return lhs == rhs
		end,
		["bin+"] = function(lhs, rhs)
			return lhs + rhs
		end,
		["bin-"] = function(lhs, rhs)
			return lhs - rhs
		end,
		["bin*"] = function(lhs, rhs)
			return lhs * rhs
		end,
		["bin/"] = function(lhs, rhs)
			return lhs / rhs
		end,
		["bin%"] = function(lhs, rhs)
			return lhs % rhs
		end,
	},
	string = {
		["bin.."] = function(lhs, rhs)
			return lhs .. rhs
		end,
		["bin*"] = function(lhs, rhs)
			return lhs:rep(rhs)
		end,
		["bin<"] = function(lhs, rhs)
			return lhs < rhs
		end,
		["bin<="] = function(lhs, rhs)
			return lhs <= rhs
		end,
		["bin=="] = function(lhs, rhs)
			return lhs == rhs
		end,
	},
	table = {
		index = function(obj, ...)
			assert(select("#", ...) > 0, "TypeError")
			local v = ...
			return obj.value[v]
		end,
		newindex = function(obj, val, ...)
			assert(select("#", ...) > 0, "TypeError")
			local k = ...
			obj.value[k] = val
		end,
	},
}

local function tryGetMeta(obj, method)
	local kind = type(obj)
	local metatable
	if kind == "table" then
		metatable = obj.mt
	else
		metatable = mt[kind]
	end
	return metatable[method]
end

local function getMeta(obj, method)
	local result = tryGetMeta(obj, method)
	assert(result, "TypeError")
	return result
end

function s.import(module, ...)
	local result = {}
	local exports = require("out." .. module)
	for _, name in ipairs {...} do
		table.insert(result, exports[name])
	end
	return unpack(result)
end

function s.call(func, ...)
	return getMeta(func, "call")(func, ...)
end

function s.toBool(value)
	if value == nil or value == false then
		return false
	else
		return true
	end
end

function s.applyBinOp(op, lhs, rhs)
	if op == "||" then
		if s.toBool(lhs) then
			return lhs
		else
			return rhs
		end
	elseif op == "&&" then
		if s.toBool(lhs) then
			return rhs
		else
			return lhs
		end
	elseif op == "==" then
		local meta = tryGetMeta(lhs, "bin==")
		if meta then
			return meta(lhs, rhs)
		else
			return lhs == rhs
		end
	elseif op == "!=" then
		return not s.applyBinOp("==", lhs, rhs)
	elseif op == ">" then
		return s.applyBinOp("<", rhs, lhs)
	elseif op == ">=" then
		return s.applyBinOp("<=", rhs, lhs)
	else
		return getMeta(lhs, "bin" .. op)(lhs, rhs)
	end
end

function s.applyUnOp(op, value)
	return getMeta(value, "un" .. op)(value)
end

function s.index(base, ...)
	return getMeta(base, "index")(base, ...)
end

function s.newindex(base, ...)
	local args = {...}
	local len = select("#", ...)
	return getMeta(base, "newindex")(base, args[len], unpack(args, 1, len - 1))
end

-- literals:

function s.table(data)
	return {
		mt = mt.table,
		value = data,
	}
end

s._nil = nil

return s