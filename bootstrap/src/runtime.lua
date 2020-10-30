local s = {}

local unpack = (_G.unpack or table.unpack)

local function bitop(a, b, oper)
	local r, m, s = 0, 2^52
	repeat
		s, a, b = a + b + m, a % m, b % m
		r, m = r + m * oper % (s - a - b), m / 2
	until m < 1
	return r
end

local mt mt = {
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
		["bin**"] = function(lhs, rhs)
			return lhs ^ rhs
		end,
		["bin|"] = function(lhs, rhs)
			return bitop(math.floor(lhs), math.floor(rhs), 1)
		end,
		["bin&"] = function(lhs, rhs)
			return bitop(math.floor(lhs), math.floor(rhs), 4)
		end,
		["bin^"] = function(lhs, rhs)
			return bitop(math.floor(lhs), math.floor(rhs), 3)
		end,
		["bin<<"] = function(lhs, rhs)
			return math.floor(lhs) * 2 ^ math.floor(rhs)
		end,
		["bin>>"] = function(lhs, rhs)
			return math.floor(math.floor(lhs) / 2 ^ math.floor(rhs))
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
		index = function(obj, k, k2)
			local function next(i)
				while true do
					i = i + 1
					local c = obj:byte(i + 1)
					if i >= #obj or s.applyBinOp("&", c, 192) ~= 128 or c <= 127 then
						break
					end
				end
				return i
			end
			local function prev(i)
				while true do
					i = i - 1
					local c = obj:byte(i + 1)
					if i <= 0 or s.applyBinOp("&", c, 192) ~= 128 or c <= 127 then
						break
					end
				end
				return i
			end
			if k == "byteLength" then
				return #obj
			elseif k == "next" then
				return next
			elseif k == "prev" then
				return prev
			elseif k == "includes" then
				return function(s)
					return obj:find(s, 1, true) ~= nil
				end
			elseif k == "popFront" then
				return function()
					return obj:sub(next(0) + 1)
				end
			elseif k == "front" then
				return function()
					return obj:sub(1, next(0))
				end
			elseif k == "empty" then
				return function()
					return #obj == 0
				end
			elseif type(k) == "number" then
				if type(k2) == "number" then
					error("TypeError")
				else
					local i = k
					while true do
						local c = obj:byte(i + 1)
						if i <= 0 or s.applyBinOp("&", c, 192) ~= 128 or c <= 127 then
							break
						end
						i = i - 1
					end
					return obj:sub(i + 1, next(i))
				end
			else
				error("TypeError")
			end
		end,
		tostring = function(obj)
			return obj
		end,
	},
	boolean = {
		tostring = function(obj)
			return tostring(obj)
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
		["bin.."] = function(lhs, rhs)
			if lhs.value.fullLength == lhs.length then
				lhs.value.value[lhs.value.fullLength] = rhs
				lhs.value.fullLength = lhs.value.fullLength + 1
				return {
					mt = mt.array,
					start = 0,
					length = lhs.value.fullLength,
					value = lhs.value,
				}
			else
				local fullLength = lhs.length + 1
				local newValue = {}
				for i = 0, fullLength - 2 do
					newValue[i] = lhs.value.value[lhs.start + i]
				end
				newValue[fullLength - 1] = rhs
				return {
					mt = mt.array,
					start = 0,
					length = fullLength,
					value = {
						fullLength = fullLength,
						value = newValue,
					},
				}
			end
		end,
		index = function(obj, ...)
			assert(select("#", ...) > 0, "TypeError")
			local k, k2 = ...
			if type(k) == "number" then
				k = k < 0 and -math.floor(-k) or math.floor(k)
				if type(k2) == "number" then
					k2 = k2 < 0 and -math.floor(-k2) or math.floor(k2)
					if k >= 0 and k < obj.length and k2 >= 0 and k2 < obj.length and k2 >= k then
						return {
							mt = mt.array,
							start = obj.start + k,
							length = k2 - k,
							value = obj.value,
						}
					else
						error("out of bounds")
					end
				else
					if k >= 0 and k < obj.length then
						return obj.value.value[obj.start + k]
					else
						error("out of bounds")
					end
				end
			else
				local members = {
					length = obj.length,
					isArray = obj.length == obj.value.fullLength,
					isSlice = obj.length ~= obj.value.fullLength,
					sort = function(func)
						if not func then
							func = function(a, b) return s.applyBinOp("<", a, b) end
						end

						local function partition(arr, low, high)
							local pivot = arr[high]
							local i = low - 1
							for j = low, high - 1 do
								if func(arr[j], pivot) then
									i = i + 1
									arr[i], arr[j] = arr[j], arr[i]
								end
							end
							arr[i + 1], arr[high] = arr[high], arr[i + 1]
							return i + 1
						end

						local function quickSort(arr, low, high)
							if low < high then
								local pi = partition(arr, low, high)
								quickSort(arr, low, pi - 1)
								quickSort(arr, pi + 1, high)
							end
						end

						quickSort(obj.value.value, obj.start, obj.start + obj.length - 1)
						return obj
					end,
				}
				local result = members[k]
				if result ~= nil then
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
				k = k < 0 and -math.floor(-k) or math.floor(k)
				if k >= 0 and k < obj.length then
					obj.value.value[obj.start + k] = val
				else
					error("out of bounds")
				end
			else
				error("TypeError")
			end
		end,
		tostring = function(obj)
			local content = {}
			for i = 0, obj.length - 1 do
				content[i + 1] = s.tostring(obj.value.value[obj.start + i])
			end
			return "[" .. table.concat(content, ", ") .. "]"
		end,
	},
	class = {
		index = function(obj, ...)
			assert(select("#", ...) > 0, "TypeError")
			local k = ...
			local function get(class, k)
				if class.fields[k] then
					return obj.fields[k]
				elseif class.decl[k] then
					return function(...)
						return class.decl[k](obj, ...)
					end
				elseif class.base then
					return get(class.base, k)
				else
					error("TypeError")
				end
			end
			return get(obj.class, ...)
		end,
		newindex = function(obj, val, ...)
			assert(select("#", ...) > 0, "TypeError")
			local function set(class, k)
				if class.fields[k] then
					obj.fields[k] = val
				elseif class.base then
					set(class.base, k)
				else
					error("TypeError")
				end
			end
			set(obj.class, ...)
		end,
		call = function(obj, ...)
			if obj.isBase then
				s.index(obj, "this")(...)
			else
				error("TypeError")
			end
		end,
		tostring = function(obj)
			return "<anonymous class>"
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
	assert(result, "TypeError: " .. method)
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

local Object = {}

Object.fields = {}

Object.decl = {
	this = function() end,
}

function s.class(base)
	local result = {}

	result.fields = {}
	result.decl = {}
	if base == nil then
		result.base = Object
	else
		result.base = base
	end

	return result
end

function s.super(instance)
	local result = instance.super
	if not result then
		result = {
			mt = mt.class,
			fields = instance.fields,
			class = instance.class.base,
			isBase = true,
		}
		instance.super = result
	end
	return result
end

function s.new(postfields, class, ...)
	local fields = {}
	local result = {
		mt = mt.class,
		fields = fields,
		class = class,
	}
	local EMPTY = {}
	for k in pairs(class.fields) do
		result.fields[k] = EMPTY
	end
	local function doFields(class)
		if class.base then
			doFields(class.base)
		end
		for k, v in pairs(class.fields) do
			if v ~= "empty" then
				result.fields[k] = v(result)
			end
		end
	end
	doFields(class)
	local i = 1
	while postfields[i] do
		local k = postfields[i]
		local v = postfields[i + 1]
		s.newindex(result, k, v)
		i = i + 2
	end
	s.index(result, "this")(...)
	for k in pairs(class.fields) do
		if result.fields[k] == EMPTY then
			error("variable '" .. k .. "' has not been initialized", 0)
		end
	end
	return result
end

function s.table(data)
	return {
		mt = mt.table,
		value = data,
	}
end

function s.array(length, data)
	local value = {}
	for i = 1, length do
		value[i - 1] = data[i]
	end
	return {
		mt = mt.array,
		start = 0,
		length = length,
		value = {
			value = value,
			fullLength = length,
		},
	}
end

s._nil = nil

return s