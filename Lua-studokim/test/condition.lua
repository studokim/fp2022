if 0 then a00 = true else a00 = false end
if 1 then a01 = true else a01 = false end
if (0 + 1) then a02 = true else a02 = false end

if "" then a10 = true else a10 = false end
if "str" then a11 = true else a11 = false end

if x then a20 = true else a20 = false end
x = nil
if x then a21 = true else a21 = false end
x = 1
if x then a22 = true else a22 = false end

if f then a30 = true else a30 = false end
function f() end
if f then a31 = true else a31 = false end

__show_env()
