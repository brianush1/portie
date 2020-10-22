local s = {}

local unpack = (_G.unpack or table.unpack)

local mt = {
	["function"] = {
		call = function(obj, ...)
			return obj(...)
		end,
		tostring = function(obj)
			return "<anonymous function>"
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
		["un-"] = function(obj)
			return -obj
		end,
		tostring = function(obj)
			return ("%.20g"):format(obj)
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
		tostring = function(obj)
			return obj
		end,
	},
	boolean = {
		tostring = function(obj)
			return obj
		end,
	},
	["nil"] = {
		tostring = function(obj)
			return "nil"
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
		tostring = function(obj)
			local content = {}
			local i = 1
			for k, v in pairs(obj) do
				content[i] = "\t[" .. s.tostring(k) .. "] = " .. s.tostring(v) .. ";\n"
				i = i + 1
			end
			return "{\n" .. table.concat(content, "") .. "}"
		end,
	},
	array = {
		index = function(obj, ...)
			assert(select("#", ...) > 0, "TypeError")
			local k = ...
			if type(k) == "number" then
				if k % 1 == 0 and k >= 0 and k < obj.length then
					return obj.value[k + 1]
				else
					return nil
				end
			else
				local methods = {
					append = function(elem)
						obj.value[obj.length + 1] = elem
						obj.length = obj.length + 1
					end,
					indexOf = function(elem)
						for i = 1, obj.length do
							if s.applyBinOp("==", obj.value[i], elem) then
								return i - 1
							end
						end
						return nil
					end,
					remove = function(index)
						for i = index + 1, obj.length - 1 do
							obj.value[i] = obj.value[i + 1]
						end
						obj.value[obj.length] = nil
						obj.length = obj.length - 1
					end,
					length = obj.length,
				}
				local result = methods[k]
				if result then
					return result
				else
					error("TypeError")
				end
			end
		end,
		newindex = function(obj, val, ...)
			assert(select("#", ...) > 0, "TypeError")
			local k = ...
			if type(k) == "number" then
				if k % 1 == 0 then
					obj.value[k + 1] = val
				end
			else
				error("TypeError")
			end
		end,
		tostring = function(obj)
			local content = {}
			for i = 1, obj.length do
				content[i] = s.tostring(obj.value[i])
			end
			return "[" .. table.concat(content, ", ") .. "]"
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

function s.tostring(obj)
	return getMeta(obj, "tostring")(obj)
end

-- literals:

function s.table(data)
	return {
		mt = mt.table,
		value = data,
	}
end

function s.array(length, data)
	return {
		mt = mt.array,
		length = length,
		value = data,
	}
end

s._nil = nil

return s