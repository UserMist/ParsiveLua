do
  getmetatable('').__index = function(str,i) return string.sub(str,i,i) end
  getmetatable('').__newindex = function(str,i,v) end
  function back(t) return t[#t] end
  function class(base, init)
    local ret = {}
    if not init and type(base) == 'function' then
      init = base
      base = nil
    elseif type(base) == 'table' then
      for i,v in pairs(base) do
        ret[i] = v
      end
      ret._base = base
    end
    ret.__index = ret

    local mt = {}
    mt.__call = function(class_tbl, ...)
      local obj = {}
      setmetatable(obj,ret)
      if init then
        init(obj,...)
      else 
        if base and base.init then
          base.init(obj, ...)
        end
      end
      return obj
    end
    ret.init = init
    ret.is = function(self, klass)
      local m = getmetatable(self)
      while m do 
        if m == klass then return true end
        m = m._base
      end
      return false
    end
    setmetatable(ret, mt)
    return ret
  end
end

-------------------------
Parser = class(
  function(self, src) 
    self.src = src
    self.pos = 1
    self.path = {}
    self.short = {}
    self.outs = nil
    self.identationBlocks = true
    self.identationThreshold = 2
    self.curlyBlocks = true
    self.includeLuacySystemFuncs = true
    
    self.getLineInfo = function(self)
      local line = 1
      local char
      for i = self.pos-1,1,-1 do
        if self.src[i] == '\n' then
          line = line+1
          char = char or self.pos-i
        end
      end
      return '(line '..(line)..', character '..(char or self.pos)..')'
    end
  end
)
----------------------------
function Parser:try()
  table.insert(self.path, self.pos) 
  return true
end

function Parser:retry()
  self.pos = self.path[#self.path]
end

function Parser:overwriteTry()
  self.path[#self.path] = self.pos
end

function Parser:endTry(item)
  if not item then
    self.pos = self.path[#self.path]
  end
  table.remove(self.path)
  return item
end

 -- Only use this in functions, that HAVE to succeed
function Parser:addError(errors, msg)
  table.insert(errors, { position = self.pos , message = msg })
  return nil
end

function Parser:out()
  self.outs = {}
  table.insert(self.short, self.outs)
end

function Parser:endOut()
  table.remove(self.short)
  self.outs = back(self)
end

function Parser:store(val)
  table.insert(back(self.short), val)
  return val
end

function Parser:skip(item)
  return true
end

function Parser:prevPos()
  return self.path[#self.path]
end

function Parser:jumpBy(delta)
  self.pos = self.pos + delta
end

function Parser:substr(offsetA,offsetB)
  return string.sub(self.src, self:prevPos()+(offsetA or 0), self.pos-1-(offsetB or 0))
end

function Parser:marchUntil(sub)
  while not self:Match(sub) do 
    if not self:Char() then return false end 
  end
  return true
end

function Parser:stepless(f, ...)
  self:try()
  local ret = f(self, ...)
  self:endTry()
  return ret
end

-- Tokenizer is allowed to look forward, but never back
function Parser:Char()
  if self.pos > #self.src then return nil end
  self.pos = self.pos+1
  return self.src[self.pos-1]
end

function Parser:CharFrom(chars)
  if self.pos > #self.src then return nil end
  
  for i=1,#chars do
    if chars[i] == self.src[self.pos] then
      self:jumpBy(1)
      return chars[i]
    end
  end
  return nil
end

function Parser:Match(sub)
  local start,len,src = self.pos, #sub-1, self.src
  if start+len <= #src and string.sub(src,start,start+len) == sub then
    --print('=========="'..sub..'"')
    self.pos=start+(len+1)
    return sub
  end
  --print('"'..sub..'"')
end

function Parser:MatchMulti(...)
  for i=1,select('#',...) do
    local ret = self:Match(select(i,...))
    if ret then
      return ret
    end
  end
end

function Parser:MatchChar(ch)
  if self.pos <= #self.src and self.src[self.pos] == ch then 
    self:jumpBy(1)
    return ch
  end
  return nil
end

function Parser:Letter() 
  return self:CharFrom("_abcdefghijklmnopqrstuvwxyzABCDEFGHIJKLMNOPQRSTUVWXYZ") 
end

function Parser:Letters()
  while self:Digit() do end
  if self:prevPos() == self.pos then return nil end
  return self:substr()
end

function Parser:Digit()
  return self:CharFrom("0123456789") 
end

function Parser:UnsignedNumber()
  self:try()
  do
    local won = false
    while self:Digit() do won = true end
    if not won then return self:endTry(nil) end
  end
  
  if self:Match('.') then
    local won = false
    while self:Digit() do won = true end
    if not won then self:jumpBy(-1) end
  end
  
  return self:endTry(tonumber(self:substr()))
end

function Parser:Number()
  self:try()
  
  local m = 1
  if self:Match('-') then m = -m end
  if self:UnsignedNumber() then return self:endTry(tonumber(self:substr())) end
  
  self:endTry()
end

function Parser:Trim()
  local won = false
  while self:CharFrom(" \t\n") do won = true end
  return won
end

function Parser:SkipSpaces()
  local won = false
  while self:CharFrom(" \t") do won = true end
  return won
end

function Parser:Keyword(s)
  self:try()
  if self:Match(s) and not (self:Letter() or self:Digit()) then return self:endTry(self:substr()) end
  self:endTry()
end

function ReturnAST(d,pre,v)
  local pre = d..pre
  if type(v) == 'string' or type(v) == 'number' then return pre..'"'..v..'"\n' end
  
  if type(v) == 'table' then
    local s = v.t and pre..'{'..v.t..'}\n' or pre..'{}\n'
    local d = d..'  '
    for k,_v in pairs(v) do
      local _s
      if k=='w' or k=='t' or k=='precedence' or k=='isPrefix' or k=='isPostfix' then
        _s = ''
      else
        _s = ReturnAST(d, (type(k)=='number' and k%1==0) and '' or ''..tostring(k)..' = ',_v)
      end
      if #_s>0 then s = s.._s end
    end
    return s
  end
  
  return pre..tostring(v)..'\n'
end

return Parser