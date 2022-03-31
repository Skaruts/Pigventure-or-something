
-- title:  Pigventure (Or Something)
-- author: Skaruts (MetalDudeBro)
-- desc:   Following from this tutorial: https://www.youtube.com/watch?v=SoFOva5FUnI
-- script: lua
-- input: mouse, keyboard
-- saveid: foo
-- version 0.4

--=--=--=--=--=--=--=--=--=--=--=--=--=--=--=--=--=--=--=--
--          TODO:
--
--      - player movement is bumpy
--      - player gets attacked when moving toward a mob, before the animation ends
--
--=--=--=--=--=--=--=--=--=--=--=--=--=--=--=--=--=--=--=--
local assert,tonum,tostr,type,setmt,getmt,pairs,ipairs=assert,tonumber,tostring,type,setmetatable,getmetatable,pairs,ipairs
local gsub,fmt,rep=string.gsub,string.format,string.rep
local conc,ins,rem=table.concat,table.insert,table.remove
local unpk = table.unpack
local abs,flr,min,max,sin,sqrt,rand=math.abs,math.floor,math.min,math.max,math.sin,math.sqrt,math.random

function box(x,y,w,h)
    return {x=x or 0,y=y or 0,w=w or 0,h=h or 0}
end
--[[ vec() - a vector2 object        (0.03) ]] local _VECMT,vec0,vec2,vecv,vec={} function vec(x,y) if not x then return vec0()end if not y then return vecv(x)end return vec2(x,y) end function vec0()return setmt({x=0,y=0},_VECMT)end function vecv(t)return setmt({x=t.x,y=t.y},_VECMT)end function vec2(x,y)return setmt({x=x,y=y},_VECMT)end _VECMT={__index=_VECMT, __tostring=function(t)return fmt("(%s,%s)",t.x,t.y)end, __mod=function(a,b)return raweq(a,b)end, __add=function(a,b)return type(b)=="number"and vec2(a.x+b,a.y+b)or vec2(a.x+b.x,a.y+b.y)end, __sub=function(a,b)return type(b)=="number"and vec2(a.x-b,a.y-b)or vec2(a.x-b.x,a.y-b.y)end, __mul=function(a,b)return type(b)=="number"and vec2(a.x*b,a.y*b)or vec2(a.x*b.x,a.y*b.y)end, __div=function(a,b)return type(b)=="number"and vec2(a.x/b,a.y/b)or vec2(a.x/b.x,a.y/b.y)end, __idiv=function(a,b)return type(b)=="number"and vec2(a.x//b,a.y//b)or vec2(a.x//b.x,a.y//b.y)end, __eq=function(a,b)return a.x==b.x and a.y==b.y end, __concat=function(a,b)return tostr(a)..tostr(b)end, floored=function(v)return vec2(flr(v.x),flr(v.y))end, ceiled=function(v)return vec2(ceil(v.x),ceil(v.y))end, rounded=function(v)return vec2(flr(v.x+0.5),flr(v.y+0.5))end, }


--[[ ton - better tonumber              (0.01) ]] function ton(v)if type(v)=="boolean" then return v and 1 or 0 end return v end
--[[ sign - get the sign of v           (0.02) ]] function sign(v)return v>0 and 1 or v<0 and -1 or 0 end
--[[ dist2 - pythagorian distance       (0.02) ]] function dist2(x,y,x2,y2)local a,b=x-x2,y-y2 return sqrt(a*a+b*b)end
--[[ wrap - wrap v around l and h       (0.02) ]]
	function wrap(v,l,h)
		-- return v > h and l or v<l and h or v
		return (v-l)%h+l -- experimental
	end
--[[ a2d - make a new 2d array          (0.01) ]] function a2d(w,h,def) def=def or 0 local t,tj={} for j=0,h-1 do t[j]={} for i=0,w-1 do t[j][i]=def end end return t end
-- [[ lerp - linear interpolate         (0.02)  ]] local function lerp(a,b,t)return a*(1-t)+b*t end
-- [[ Draw a test grid                          ]] local function draw_test_grid()for i=0,240,8 do line(i,0,i,136,7)end for j=0,136,8 do line(0,j,240,j,7)end line(240//2,0,240//2,136,15)line(0,136//2,240,136//2,15)end
--[[ Print w/ shadow                    (0.05) ]] local shad_c=15 function prints(t,x,y,c,fixw,ofx,ofy)local xc,yc,ofx,ofy=1,1,ofx or 0,ofy or 0print(t,x*8+xc+ofx+1,y*8+yc+ofy+1,shad_c,fixw,1)print(t,x*8+xc+ofx,y*8+yc+ofy,c,fixw,1)end
--[[ Print w/ shadow centered           (0.07) ]] function printc(t,x,y,c,fixw,ox,oy)local xc,yc,c,l=1,1,c or 15,print(t,-999,-999)x=x==nil and(240-l)//2or x*8y=y==nil and 136//2-8//2or y*8print(t,x+xc+1,y+yc+1,shad_c,fixw)print(t,x+xc,y+yc,c,fixw)end
--[[ printo/printog - print w/ outline  (0.01) ]] local _dr={-1,-1,0,-1,1,-1,0,-1,0,1,-1,1,0,1,1,1}function printog(t,x,y,c,oc,fixw,ofx,ofy)oc,ofx,ofy=oc or 12,ofx or 0,ofy or 0 local tx,ty=x8+ofx+1,y*8+ofy+1 for i=1,15,2 do print(t,tx+_dr[i],ty+_dr[i+1],oc,fixw,1)end print(t,tx,ty,c or 0,fixw,1)end function printo(t,x,y,c,oc,fixw)oc=oc or 12 for i=1,15,2 do print(t,x+_dr[i],y+_dr[i+1],oc,fixw,1)end print(t,x,y,c or 0,fixw,1)end
--[[ tracef - trace formatted           (0.01) ]] function tracef(...)trace(fmt(...))end
--[[ tracec - trace csv arguments       (0.01) ]] function tracec(...)trace(conc({...},",").."\n")end
--[[ trace2d - trace a 2d array         (0.02) ]] function trace2d(a,sep,i0) sep=sep or""local s,w,c,aj=i0 and 0 or 1,#a[1] for j=s,#a do c,aj={},a[j] for i=s,w do c[i+1-s]=aj[i] end trace(conc(c,sep)) end end
--[[ all - ipairs but w/o idx           (0.01) ]] function all(t)local i,n=0,#t return function()i=i+1if i<=n then return t[i]end end end
--[[ Python-like range() (0-indexed)    (0.01) ]] function range(b,e,s)if not b or b==e or s==0 then error("invalid range",2)end if not e then e,b=b,0 end if not s then s=e>b and 1 or -1 end if (e<b and s>0) or (e>b and s<0) then error("invalid step") end local i=b-s return e>b and function()i=i+s if i<e then return i end end or function()i=i+s if i>e then return i end end end
--[[ find - find o in t                 (0.01) ]] function find(t,o)for i=1,#t do local v=t[i]if v==o then return i,v end end end
--[[ del - delete o from t              (0.01) ]] function del(t,o)local i=find(t,o)if not i then return end rem(t,i)return o end
--[[ fdel - fast del o from t           (0.01) ]] function fdel(t,o)local i,s=find(t,o),#t if not i then return end if i==s then t[s]=nil else local l=t[s]t[s],t[i]=nil,nil t[i]=l end return o end
--[[ frem - fast remove from t at i     (0.01) ]] function frem(t,i)local o,s=t[i],#t if i==s then t[s]=nil else local l=t[s]t[s],t[i]=nil,nil t[i]=l end return o end
--[[* fins - fast insert o in t         (0.01) ]] function fins(t,o,i)if not i then t[#t+1]=o else t[#t+1]=t[i]t[i]=o end end
--[[ clamp - keep v between l and h     (0.02) ]] function clamp(v,l,h)return max(l,min(v,h))end
--[[ Debugging utility                  (0.11) ]] local DBG_KEY,frm=41,1 local dbg={active=false, crammed=false, col=12, fix_w=true, reg={}, h=0,w=0,vals=nil, toggle=function(t)t.active=not t.active end, cram_text=function(t,b)t.crammed=b end, draw=function(t) frm=frm+1 if frm>500 then frm=1 for k,_ in pairs(t.reg)do t.reg[k]=0 end end if t.active then if t.crammed then local w=prints(t.vals,t.fix_w) rect(0,0,w+8,t.h*8,1) prints(t.vals,0,0,t.col,t.fix_w) t.vals=""else local w=t.w*8-t.w*2 rect(0,0,w+8,t.h*8+8,1) for i=1,#t.vals do prints(t.vals[i],0,i-1,t.col,t.fix_w) end t.vals,t.w={},0 end t.h=0 end end, } dbg.vals=dbg.crammed and""or{} function monitor(k,v,ak) local t=dbg if t.active then if t.crammed then if v==nil then t.vals=conc({t.vals,k,'\n'}) elseif k~=""then if ak then k=k..rep(' ',ak-#k) end t.vals=conc({t.vals,k,tostr(v),'\n'}) else t.vals=conc({t.vals,tostr(v),'\n'}) end else local s if v==nil then s=k elseif k~=""then if ak then k=k..rep(' ',ak-#k) end s=conc({k,tostr(v)}) else s=tostr(v) end t.vals[#t.vals+1]=s if #s>t.w then t.w=#s end end t.h=t.h+1 end end function bm(name,f) local tm=time() f() monitor(name,fmt("%.2f",time()-tm).."ms") end function bma(name,f) local reg,tm1,tm2,str=dbg.reg if not reg[name]then reg[name]=0 end tm1=time() f() tm2=time()-tm1 str=fmt("%.2f",tm2).."ms"reg[name]=reg[name]+tm2 str=str..rep(' ',9-#str)..fmt("%.2f",reg[name]/frm).."ms"monitor(name..rep(' ',11-#name),str) end
--[[ lifoq() - a simple LIFO queue      (0.01) ]] local _QUEMT={} function lifoq()return setmt({x=0},_QUEMT)end _QUEMT={__index=function(t,k) return _QUEMT[k]end, push=function(t,o)t[#t+1]=o end, peek=function(t)return t[#t]end, pop=function(t) local i=t[#t] t[#t]=nil return i end, has=function(t,o) for i=1,#t do if t[i]==o then return i end end end, clear=function(t) for i=#t,1,-1 do t[i]=nil end end, }
--[[ tm_check - update time stuff       (0.01) ]]
	local t,t1,t2,dt,tsecs,tm_check=0,time()
	function tsecs(tm) return (tm or time())/1000 end
	function tm_check()
		t=t+1
		t2=time()
		dt=t2-t1/1000 -- delta time. Never had a use for it, lol
		t1=t2
	end
--[[ pal - palette swapping             (0.05) ]] local _scr_pal,pal={} function pal(c1,c2,p) if c2 then if not p then poke4(0x3FF0*2+c1,c2) else _scr_pal[c1]=c2 _scr_pal[16]=true end elseif not c1 then local pok=poke4 for i=0,15 do pok(0x3FF0*2+i,i)end elseif c1==true then if _scr_pal[16]then local pek,pok=peek4,poke4 for i=0,136*240-1 do local c=pek(i) if _scr_pal[c]then pok(i,_scr_pal[c]) end end _scr_pal[16]=false end end end
--[[ sprp - spr w/ palette swapping     (0.03) ]] local _swapc=13 function sprp(id,x,y,c,flp) pal(_swapc,c) spr(id,x,y,-1,1,flp) pal() end
--[[ flip - mimics pico-8 flip          (0.01) ]]
	local flip_tm,_doloop,_TIC,_co_loop,flip=0
	local _cstat,_ocres,_cnew,_cyld=coroutine.status,coroutine.resume,coroutine.create,coroutine.yield
	-- function coroutine.xpcall(co)
	function coxpcall(co)
		local output = {coroutine.resume(co)}
		if output[1] == false then
			return false, output[2], debug.traceback(co)
		end
		return table.unpack(output)
	end
	function flip()
		-- print(flip_tm,5,16,0)
		-- time passed in frame so far
		flip_tm = time()-t2  -- TODO: currently not in use, check if really needed
		-- print(tm_passed,5,16,4)
		_cyld(_co_loop)
	end
	function _cres(co)

		local v,e,tb = coxpcall(_co_loop)
		if not v then error(e .. "\n" .. tb,4)end

		-- local v,e=_ocres(_co_loop)
		-- if not v then error(e,4)end
	end
	function _doloop()
		if _co_loop ~= nil then
			local status = _cstat(_co_loop)
			if status == "dead" then
				_co_loop = _cnew(_TIC)
				_cres(_co_loop)
			elseif status == "suspended" then
				-- local tm=0
				-- while tm<flip_tm do tm=time()-t2 end
				_cres(_co_loop)
			end
		else
			_co_loop = _cnew(_TIC)
			_cres(_co_loop)
		end
	end
	function TIC()
		tm_check()
		_doloop()
		pal(true)
	end
--[[ wait - halts game for 'n' frames   (0.01) ]] function wait(n) if not n then return end while n>0 do n=n-1 flip() end end
--[[ fade_in/fade_out                   (0.01) ]] local _fdefinc,_fsteps,_fpc=0.05,6,1 local _flkt={{ 1, 8, 0, 0, 0, 0}, { 2, 1, 8, 0, 0, 0}, { 3, 2, 1, 8, 0, 0}, { 4, 3, 2, 1, 8, 0}, { 5, 6, 7,15, 8, 0}, { 6, 7,15, 8, 0, 0}, { 7,15, 8, 0, 0, 0}, { 8, 0, 0, 0, 0, 0}, { 9, 8, 0, 0, 0, 0}, {10, 9, 8, 0, 0, 0}, {11,10, 9, 8, 0, 0}, {12,13,14,15, 8, 0}, {13,14,15, 8, 0, 0}, {14,15, 8, 0, 0, 0}, {15,15, 8, 0, 0, 0}, } local function screen_state() local t,pek={},peek4 for i=1,240*136 do t[i]=pek(i-1)end return t end local function _calc_fade() local p,s=clamp(_fpc,0,1),1/_fsteps local i=flr(p/s)+1 for n=1,15 do pal(n, _flkt[n][i], true) end end local function prep_fade(dir) if     dir == "in"  then _fpc=1 elseif dir == "out" then _fpc=0 end end local function fade(dir, spd, scr_state) spd=spd or _fdefinc if     dir == "in"  then _fpc=max(0, _fpc-spd) elseif dir == "out" then _fpc=min(1, _fpc+spd) end _calc_fade() end local function check_fade(dir) if     dir == "in"  then  if _fpc>0 then fade(dir) end elseif dir == "out" then  if _fpc<1 then fade(dir) end end end local function fadeb(dir, spd, wait_in, wait_out, scr_state) spd,wait_in,wait_out=spd or _fdefinc,wait_in or 0,wait_out or 0 wait(wait_in) local pek,pok=peek4,poke4 if not scr_state then scr_state={} for i=1,240*136 do scr_state[i]=pek(i-1)end end if dir=="in" then _fpc=1 while _fpc>0 do _fpc=max(0,_fpc-spd) for i=1,240*136 do pok(i-1,scr_state[i])end _calc_fade() flip() end elseif dir=="out" then _fpc=0 while _fpc<1 do _fpc=min(1,_fpc+spd) if _fpc<1 then for i=1,240*136 do pok(i-1,scr_state[i]) end end _calc_fade() flip() end end wait(wait_out) end

--=--=--=--=--=--=--=--=--=--=--=--=--=--=--=--=--=--=--=--

--=--=--=--=--=--=--=--=--=--=--=--=--=--=--=--=--=--=--=--
--      VARIABLES
	local BTN_HOLD,BTN_PERIOD = 10,4

	-- local
	-- ID_LABEL, ID_SEPARATOR=
	-- 1,        2

	-- these shouldn't be here, but stuff isn't working if they're not...
	function menu_item(str, col, active)
		return {
			-- id=ID_LABEL,
			str=str,
			col=col or 13,
            active = active==nil and true or active,
		}
	end
	function menu_separator(char, reps, col)
		local s = menu_item(rep(char or '-', reps or 16), col or 15, false)
        s.separator = true
		return s
	end
	-- function ui_text(str, col) return {str=str, col=col} end


	local MW,MH = 30, 17
	local states,anim_state = lifoq()
	local player
	local floaters,mobs,dead_mobs={},{},{}
	local sprites={112,224,208}
	local mob_atk,mob_hp,mob_los={1,1},{5,2},{4,4}
	local item_names = {
		"PpgT[|/ sword",
		"armor",
		"potion",
	}
	local
	ID_PLR,ID_SLM,ID_DOG=
	1,     2,     3
	local ids_by_spr={
		[sprites[1]]=ID_PLR,
		[sprites[2]]=ID_SLM,
		[sprites[3]]=ID_DOG,
	}
	local PLR_SPAWN=248

	local eqp,inv={},{}

	--u/d/l/r /tl/tr/bl/br/
	local
	x_dirs,y_dirs=
	{ 0, 0,-1, 1, -1, 1,-1, 1},
	{-1, 1, 0, 0, -1,-1, 1, 1}

	local chests={[236]=true,[237]=true}
	local doors={[233]=true,[234]=true,[235]=true}
	local vases={[238]=true,[239]=true}
	local stairs={[232]=true}
	local plates={
		[200]={
			menu_item("Your doom lies"),
			menu_item("ahead, puny pig!"),
			menu_item(""),
			menu_item("Tread willfully"),
			menu_item("for I am starving!"),
			menu_item(""),
			menu_item(" -- The Butcher")
		},
		[201]={
			menu_item("Open that chest"),
			menu_item("and you shall"),
			menu_item("   receive.")
		},
		[202]={
            menu_item("To victory"),
            menu_item(" ------->")
        },
	}



	local move_anim_spd,spr_anim_spd=0.1,4
	local fog = a2d(30, 17, 1)
	local distmap = a2d(30, 17, -1)
	-- local animations={}
--=--=--=--=--=--=--=--=--=--=--=--=--=--=--=--=--=--=--=--

local _GSMT,gstate={}
function gstate(name,update,draw)
	return setmt({name=name,upd=update,drw=draw},_GSMT)
end
_GSMT={
	__index=_GSMT,
}
local function new_play_state()
	prep_fade("in")
	unfog_map()
	-- compute_distmap(player.x,player.y)
	return gstate( "play_state", update_game, draw_game )
end
local function new_anim_state()      return gstate( "anim_state", update_anims, draw_game ) end
local function new_inv_state()       return gstate( "inv_state", update_inv, draw_game ) end
local function new_game_over_state()
	prep_fade("in")
	return gstate( "gameover_state", update_gameover, draw_gameover )
end


--=--=--=--=--=--=--=--=--=--=--=--=--=--=--=--=--=--=--=--
--      TOOLS


	function get_frame(idx, spd, max_frames)
		return idx+(tsecs()*spd)%max_frames
	end

--=--=--=--=--=--=--=--=--=--=--=--=--=--=--=--=--=--=--=--

--=--=--=--=--=--=--=--=--=--=--=--=--=--=--=--=--=--=--=--
--      UI
	local windows={}
	local talkwindow, hpwindow, invwindow, statswindow, usewindow, currwindow

	function new_window(x,y,w,h,txt)
		local w={x=x,y=y,w=w,h=h,txt=txt}
		ins(windows,w)
		return w
	end


	-- function menu_sep(w,char) return {txt=rep(char,w), col=14} end

	function draw_ui()
	--  for _,w in ipairs(windows) do


		for win in all(windows) do
			local x,y,w,h=win.x,win.y,win.w,win.h
			rect(x,y,w,h,0)
			rectb(x+1,y+1,w-2,h-2,13)

			x=x+4
			y=y+4
			clip(x,y,w-8,h-8)
			if win.cur then x=x+6 end
			for i=1,#win.txt do
				local str=win.txt[i].str
				local col=win.txt[i].col

				if i~=win.cur then
				    print(str, x, y, col, 1, 1, true)
                else
                    print(str, x, y, 4, 1, 1, true)
					spr(257, x-6+sin(tsecs()*10), y, 0, 1, 0)--, cell_width, cell_height)
				end
				y=y+6
			end
			clip()

			if win.btn then
				local s=sin(time()*0.015)
				-- printo("[x]",win.x+win.w-11,win.y+win.h-5,13,0)
				spr(272,win.x+win.w-12,win.y+win.h-5+s,1)
				spr(272,win.x+win.w-10,win.y+win.h-5+s,1)
				spr(256,win.x+win.w-11,win.y+win.h-5+s,1)
			end

			if win.dur then
				win.dur=win.dur-1
				if win.dur<=0 then
					win.txt = {}
					local dif=win.w/3
					win.x=win.x+dif/2
					win.w=win.w-dif
					if win.w<=3 then
						del(windows,win)
					end
				end
			end
		end
	end

	function show_msg(tx,dur)
		tx=" "..tx.." "
		local w=print(tx)+7
		local h=14
		local x=120-w/2
		local y=68-h/2
		local win=new_window(x,y,w,h,{tx})
		win.dur=dur
	end

	function show_dialog(strs)
		local w=94
		local h=#strs*6+9
		local x=16
		local y=50
		talkwindow=new_window(x,y,w,h,strs)
		talkwindow.btn=true
	end

	function show_plate_msg(tile)
		if     tile==202 then show_dialog(plates[tile]) -- directions
		elseif tile==201 then show_dialog(plates[tile]) -- POI descriptions
		elseif tile==200 then show_dialog(plates[tile]) -- level/quest description
		end
	end

--=--=--=--=--=--=--=--=--=--=--=--=--=--=--=--=--=--=--=--

--=--=--=--=--=--=--=--=--=--=--=--=--=--=--=--=--=--=--=--
--      OBJECTS
	local mob_names={"player", "slime", "dog"}
	function new_mob(tp,x,y)
		local t={
			name     = mob_names[tp],
			x        = x,
			y        = y,
			ox       = 0,
			oy       = 0,
			sox      = 0,
			soy      = 0,

			sprite   = sprites[tp],
			col      = 4,
			flp      = false,

			hp       = mob_hp[tp],
			hp_max   = mob_hp[tp],
			atk      = mob_atk[tp],
			los      = mob_los[tp],

			flash    = 0,
			tm       = 0,
			f_anim   = nil,

			f_task   = ai_wait,
			tx       = nil,
			ty       = nil,
			tcooldown=0,
			-- fovmap=a2d(1),
		}
		ins(mobs,t)
		return t
	end

	function new_floater(txt,x,y,c,ofx,ofy)
		ins(floaters,{
			txt=txt,
			x=x*8+(ofx or 0),
			y=y*8+(ofy or 0),
			c=c,
			ty=y*8-(rand()*5+8), -- target_y
			tm=0})
	end
--=--=--=--=--=--=--=--=--=--=--=--=--=--=--=--=--=--=--=--

--=--=--=--=--=--=--=--=--=--=--=--=--=--=--=--=--=--=--=--
--      MAP STUFF
	function get_mob(x,y)
		for m in all(mobs) do
			if m.x==x and m.y==y then return m end
		end
	end

	function inbounds(x,y)
		return x>=0 and y>=0 and x<30 and y<17
	end

	function walkable(tile)
		return not fget(tile,0)
	end

	function is_obstacle(x,y)
		if inbounds(x,y) then
			return fget(mget(x,y),0) or get_mob(x,y)
		end
		return false
	end
	function blocks_sight(x,y)
		return fget(mget(x,y),2)
	end

	function LOS(x1,y1,x2,y2)
		if dist2(x1,y1,x2,y2) == 1 then return true end

		local first,sx,sy,dx,dy=true
		dx=abs(x2-x1)
		dy=abs(y2-y1)
		sx=sign(x2-x1)
		sy=sign(y2-y1)

		local err,e2=dx-dy

		while not (x1==x2 and y1==y2) do
			if not first and blocks_sight(x1,y1) then return false end
			first = false
			e2=err+err
			if e2 > -dy then
				err=err-dy
				x1=x1+sx
			end
			if e2 < dx then
				err=err+dx
				y1=y1+sy
			end
		end
		return true
	end
--=--=--=--=--=--=--=--=--=--=--=--=--=--=--=--=--=--=--=--



--=--=--=--=--=--=--=--=--=--=--=--=--=--=--=--=--=--=--=--
--      INVENTORY
    local invp = box(5,5,84,62)

    local stats_h,inv_h = 13,62

	function show_inv()
		states:push(new_inv_state())
		local menu,wep,armr={},eqp[1],eqp[2]

		ins(menu, (wep  and menu_item(item_names[wep], nil, true)  or menu_item('[weapon]', 15, false)))
		ins(menu, (armr and menu_item(item_names[armr], nil, true) or menu_item('[armor]',  15, false)))
		ins(menu, menu_separator())
		for i=1,6 do
			local item=inv[i]
			ins(menu, item and menu_item(item_names[item], nil, true) or menu_item('...', 15, false))
		end

        -- statswindow = new_window(5,5,84,13,{menu_item("ATK: 1   |  DEF:  1")})
        -- invwindow = new_window(5,16,84,62,menu)
        statswindow = new_window(invp.x, invp.y,         invp.w, stats_h, {menu_item("ATK: 1   |  DEF:  1")})
        invwindow   = new_window(invp.x, invp.y+stats_h, invp.w, invp.h,       menu)
        for i=1, #menu do
            if menu[i].active then
                invwindow.cur = i
                break
            end
        end
        currwindow = invwindow
	end

    function show_use()
        local x = invp.x+invp.w
        local y = invp.y+stats_h+(invwindow.cur-1)*6
        usewindow = new_window(x, y, 36, 25, {
            menu_item("use"),
            menu_item("throw"),
            menu_item("drop"),
        })
        usewindow.cur = 1
        currwindow = usewindow
    end

	function take_item(item)
		local i=find_free_inv_slot()
		if i then
			inv[i]=item
			return true
		end
	end


	function find_free_inv_slot()
		for i=1,6 do
			if not inv[i] then return i end
		end
	end
	take_item(1)
	take_item(2)
    take_item(2)
	take_item(3)
    take_item(2)
    take_item(3)

--=--=--=--=--=--=--=--=--=--=--=--=--=--=--=--=--=--=--=--

--=--=--=--=--=--=--=--=--=--=--=--=--=--=--=--=--=--=--=--
--      INPUT
	function get_btn()
		for i in range(4) do
			if btn(i) then return i end
		end
	end

	function do_btn(b)
		-- if not b then return end
		if b and b<4 then
			move_player(x_dirs[b+1],y_dirs[b+1])
			return
		elseif btnp(6) then
			show_inv()
		end
		-- menu btns
	end

	--[[function do_btn_buffer()
		if not btn_buffer then
			btn_buffer=get_btn()
		end
	end]]
--=--=--=--=--=--=--=--=--=--=--=--=--=--=--=--=--=--=--=--

--=--=--=--=--=--=--=--=--=--=--=--=--=--=--=--=--=--=--=--
--      DRAW
	-- function get_pal_map()
	-- 	local t={}
	-- 	for i=1,15 do
	-- 		t[i]=peek4(0x3FF0*2+i)
	-- 	end
	-- 	return t
	-- end

	function draw_fog()
		for j=0,MH-1 do
			for i=0,MW-1 do
				if fog[j][i] == 1 then
					rect(i*8, j*8, 8, 8, 0)
				end
			end
		end
	end

	function draw_distances()
		for j=0, MH-1 do
			for i=0,MW-1 do
				if distmap[j][i]>0 then
					-- print(text, x, y, col, fixed, scale, smallfont) -> text width
					print(distmap[j][i], i*8, j*8, 2, nil,1,true)
				end
			end
		end
	end

	-- local game_starting=true
	function draw_game()
		cls(0)
		map()
		draw_dead_mobs()
		draw_mobs()
		draw_fog()
		-- draw_distances()

		-- trace(tostring(states:peek().fst_frame) .. "...")
		check_fade("in")
		-- trace("---" .. tostring(states:peek().fst_frame))
		draw_floaters()
		draw_ui()
		is_player_dead()
	end

	function draw_gameover()
		cls(0)
		printc("U DEAD!",_,_,12)

		check_fade("in")

		prints("Press X to restart",1,15,12)
		if btnp(5) then
			fadeb("out", 0.05)
			reset()
		end
		-- print(,50,50,12)
	end

	function draw_mobs()
		for i=#mobs,1,-1 do
			draw_mob(mobs[i])
		end
	end

	function draw_mob(m, dead)
		local c=4
		if m.flash>0 or dead then
			m.flash=m.flash-1
			c=13
		end
		local spr = dead and m.sprite or get_frame(m.sprite,spr_anim_spd,4)
		sprp(spr,  m.x*8+m.ox,  m.y*8+m.oy,  c, m.flp)
	end

	function draw_dead_mobs()
		for m in all(dead_mobs) do
			if sin(time()*0.05)>0 then
				draw_mob(m, true)
			end
			m.dur=m.dur-1
			if m.dur <=0 then del(dead_mobs,m)end
		end
	end

	function draw_floaters()
		for f in all(floaters) do
			printo(f.txt,f.x,f.y,f.c,0)
		end
	end
--=--=--=--=--=--=--=--=--=--=--=--=--=--=--=--=--=--=--=--

--=--=--=--=--=--=--=--=--=--=--=--=--=--=--=--=--=--=--=--
--      UPDATE
	-- curr_state = "fade_in"
	-- function switch_state(st, force_it, ...)
	--     if st ~= curr_state or force_it then
	--         if     st == "game_play"    then _upd,_drw=update_game,draw_game
	--         elseif st == "game_animate" then _upd,_drw=update_anims,draw_game
	--         elseif st == "fade_in"      then start_fade_in(...)
	--         elseif st == "fade_out"     then start_fade_out(...)
	--         elseif st == "game_over"    then _upd,_drw=update_gameover,draw_gameover
	--         end
	--         curr_state = st
	--     end
	-- end

	function update_talk_window()
		if talkwindow~=nil then
			if btnp(5) then
				talkwindow.btn=nil
				talkwindow.dur=0
				talkwindow=nil
			end
		end
	end

	function update_ui()
		update_hp_window()
		update_talk_window()

	end

	--local btn_buffer
	function update_game()
		if not talkwindow then
			local b = get_btn()
			if b and b<4 then
				move_player(x_dirs[b+1],y_dirs[b+1])
				return
			elseif btnp(6) then
				show_inv()
			end
			do_floaters()
		end
		update_ui()
	end

	function update_hp_window()
		hpwindow.txt[1].str = "HP " .. player.hp .. "/" .. player.hp_max
		if     player.hp > player.hp_max/3*2 then hpwindow.txt[1].col=5
		elseif player.hp > player.hp_max/3   then hpwindow.txt[1].col=3
		else                                      hpwindow.txt[1].col=2
		end
		local hpy = player.y < 8 and 136-8-hpwindow.h+3 or 5
		hpwindow.y = hpwindow.y+(hpy-hpwindow.y)/5
	end

	function move_menu(w)
		local dir = ton(btnp(1,BTN_HOLD,BTN_PERIOD)) - ton(btnp(0,BTN_HOLD,BTN_PERIOD))
		if dir ~= 0 then
            repeat
                w.cur = wrap(w.cur+dir, 1, #w.txt)
            until w.txt[w.cur].active
        end
	end

	function update_inv()
		move_menu(currwindow)
		if btnp(4) then
            if currwindow == invwindow then
    			states:pop()
    			invwindow.dur=0
    			statswindow.dur=0
    			invwindow=nil
    			statswindow = nil
            elseif currwindow == usewindow then
                usewindow.dur = 0
                usewindow = nil
                currwindow = invwindow
            end
        elseif btnp(5) then
            if currwindow == invwindow then
                show_use()
            elseif currwindow == usewindow then

            end
		end
	end




	function update_gameover()

	end


	function update_anims()
		-- trace("updating anims")
		-- local n=0
		for m in all(mobs) do
			-- n=n+1
			if m.f_anim then
				m.tm=min(1,m.tm+move_anim_spd)
				m.f_anim(m)
				if m.tm >= 1 then m.f_anim = nil end
			end
		end
		if player.tm>=1 then
			states:pop() -- remove this current state
		end
		-- is_player_dead()
		do_floaters()
		update_hp_window()
	end

	function do_ai()
		for m in all(mobs) do
			if m~=player then
				m.f_anim=nil
				m:f_task()
			end
		end
	end

	function ai_sees_target(m, x,y)
		if dist2(m.x,m.y,x,y)<=m.los and LOS(m.x, m.y, x, y) then
			m.tcooldown=5
			return true
		end
	end
	function ai_wait(m)
		if ai_sees_target(m, player.x, player.y) then
			m.f_task = ai_attack
			new_floater("!",m.x,m.y,4,2)
			m.tx=player.x
			m.ty=player.y
		end
	end
	function ai_attack(m)
		local dx,dy
		if dist2(m.x,m.y,player.x,player.y)==1 then
			-- attack
			dx,dy=player.x-m.x,player.y-m.y
			mob_bump(m,dx,dy)
			hit_mob(m,player)
			sfx(57)
		else
			-- move
			if ai_sees_target(m, player.x, player.y) then
				m.tx = player.x
				m.ty = player.y
			else
				m.tcooldown=m.tcooldown-1
			end
			-- if not (m.tx and m.ty) or m.x == m.tx and m.y == m.ty then
			if m.tcooldown<=0 or m.x == m.tx and m.y == m.ty then
				m.f_task = ai_wait
				new_floater("?",m.x,m.y,4,2)
			else
				local best_dist,best_dx,best_dy=999,0,0
				compute_distmap(m.tx,m.ty)
				local dist_cands={}
				for i=1,4 do
					dx,dy=x_dirs[i],y_dirs[i]
					local destx,desty=m.x+dx,m.y+dy
					if not is_obstacle(destx,desty) then
						local dst = distmap[desty][destx]
						if dst >= 0 then
							if dst<best_dist then
								dist_cands={}
								best_dist=dst
							end
							if dst==best_dist then
								ins(dist_cands, {dx=dx,dy=dy,tx=destx,ty=desty})
							end
						end
					end
				end
				if #dist_cands > 0 then
					local lowest_d=999
					for c in all(dist_cands) do
						local d = dist2(c.tx, c.ty, m.tx, m.ty)
						if d < lowest_d then
							lowest_d,best_dx,best_dy=d,c.dx,c.dy
						end
					end
					mob_walk(m,best_dx,best_dy)
				end
			end
		end
	end

	function do_floaters()
		for f in all(floaters) do
			-- f.y = lerp(f.y, f.ty, 0.2)
			f.y = f.y + (f.ty-f.y) /10
			f.tm = f.tm + 1
			if f.tm>70 then fdel(floaters,f)end
		end
	end

	function is_player_dead()
		if player.hp<=0 then
			player.col=2
			states:pop()
			states:push(new_game_over_state())
			fadeb("out", 0.05, 60)
		end
	end

	function compute_distmap(x,y)
		local cands,step={},0
		distmap=a2d(30, 17, -1)
		ins(cands, {x=x,y=y})
		distmap[y][x] = step
		repeat
			step=step+1
			local newcands = {}
			for c in all(cands) do
				for d=1, 4 do
					local tx = c.x+x_dirs[d]
					local ty = c.y+y_dirs[d]
					if inbounds(tx,ty) and distmap[ty][tx]==-1 then
						distmap[ty][tx] = step
						if walkable(mget(tx,ty)) then
							ins(newcands, {x=tx,y=ty})
						end
					end
				end
			end
			cands=newcands
		until #cands==0
	end


--
--
--
--
--
--                 2. 2
--              2. 1. 1  1.
--              2  1  0  1  2
--              2. 1. 1  1.
--                    2
--
--
--
--
--
--





--=--=--=--=--=--=--=--=--=--=--=--=--=--=--=--=--=--=--=--


--=--=--=--=--=--=--=--=--=--=--=--=--=--=--=--=--=--=--=--
--      TURN
	function anim_walk(m)
		m.ox=m.sox*(1-m.tm)
		m.oy=m.soy*(1-m.tm)
	end

	function anim_bump(m)
		local tm=m.tm>0.5 and 1-m.tm or m.tm
		m.ox=m.sox*tm
		m.oy=m.soy*tm
	end

	function mob_walk(m,dx,dy)
		mob_flip(m,dx)
		m.x=m.x+dx
		m.y=m.y+dy
		m.sox=-dx*8 -- TODO: this could not be *8. The offset
		m.soy=-dy*8 --       could be between 0 and 1
		m.ox=m.sox
		m.oy=m.soy
		m.tm=0
		m.f_anim=anim_walk
	end

	function mob_bump(m,dx,dy)
		mob_flip(m,dx)
		m.sox=dx*8
		m.soy=dy*8
		m.ox=0
		m.oy=0
		m.tm=0
		m.f_anim=anim_bump
	end

	function mob_flip(m,dir)
		if dir~=0 then m.flp = dir<0 and 1 or 0 end
	end

	function unfog_tile(x,y)
		fog[y][x] = 0
		if not blocks_sight(x,y) then
			for i=1,8 do
				local tx,ty=x+x_dirs[i],y+y_dirs[i]
				if blocks_sight(tx,ty) then
					fog[ty][tx] = 0
				end
			end
		end
	end

	function unfog_map()
		for j=0, MH-1 do
			for i=0, MW-1 do
				if fog[j][i]==1
				and dist2(player.x,player.y,i,j) <= player.los
				and LOS(player.x,player.y, i,j) then
					unfog_tile(i,j)
				end
			end
		end
		-- for j=0, MH-1 do
		-- 	for i=0, MW-1 do
		-- 		if fog[j][i] == 1 and mget(i,j) == 8 then -- blocks_sight(i,j) then
		-- 			if any_opaque_neibs_in_sight(i,j) then
		-- 				fog[j][i] = 0
		-- 			end
		-- 		end
		-- 	end
		-- end
	end


	function move_player(dx,dy)
		local dstx,dsty=player.x+dx,player.y+dy
		-- switch_state("plr_turn")
		-- pt=0

		local tile=mget(dstx,dsty)
		if not is_obstacle(dstx,dsty) then  -- clear
			sfx(63)
			mob_walk(player,dx,dy)
			do_ai()

			-- fov_recompute(player.fovmap, player.x, player.y, 10)
			-- update_plr_turn()
		else                                -- obstacle
			mob_bump(player,dx,dy)

			local m=get_mob(dstx,dsty)
			if not m then
				if fget(tile,1) then
					trig_bump(tile,dstx,dsty)
				end
			else
				sfx(58)
				hit_mob(player,m)
			end
			-- update_plr_turn()
			do_ai()
		end
		unfog_map()
		compute_distmap(player.x,player.y)
		states:push(anim_state)
		-- states:push(new_anim_state())
		-- switch_state("game_animate")
	end

	function hit_mob(a,b)
		local dmg=a.atk
		b.hp=b.hp-dmg
		b.flash=8

		if b~=player then new_floater(-dmg,b.x,b.y,3,  0,-4)
		else              new_floater(-dmg,b.x,b.y,2,  0,-4)
		end
		check_dead(b)
	end

	function check_dead(m)
		if m.hp<=0 then
			m.sprite = get_frame(m.sprite,spr_anim_spd,4)
			ins(dead_mobs,m)
			m.dur=45
			if m~=player then
				fdel(mobs,m)
			end
			mset(m.x,m.y,1)
		end
	end

	function trig_bump(tile,x,y)
		if     plates[tile] then -- stone plate
			show_plate_msg(tile)
		elseif vases[tile] then  -- vases
			mset(x,y,tile+16)
			sfx(59)
		elseif chests[tile] then -- chests
			mset(x,y,tile+16)
			sfx(61)
		elseif doors[tile] then  -- door
			mset(x,y,tile+16)
			sfx(62)
		elseif stairs[tile] then -- stairs

		end
	end
--=--=--=--=--=--=--=--=--=--=--=--=--=--=--=--=--=--=--=--


--=--=--=--=--=--=--=--=--=--=--=--=--=--=--=--=--=--=--=--
--          INIT / LOOP
	function init()
		spawn_mobs()
		-- startgame()
		states:push(new_play_state())
		hpwindow=new_window(5,5,40,13, {menu_item("HP 5/5",5)})
		anim_state = new_anim_state()
	end

	function spawn_mobs()
		mobs={}
		for j=0,16 do
			for i=0,29 do
				local s=mget(i,j)
				if s==PLR_SPAWN then
					player=new_mob(ID_PLR,i,j)
				elseif ids_by_spr[s] then
					new_mob(ids_by_spr[s],i,j)
					mset(i,j,1)
				end
			end
		end
	end

	-- function startgame()
	--  player=nil
	--  pt=0
	--  floaters={}
	--  windows={}
	--  talkwindow=nil
	--  _upd=update_game
	--  _drw=draw_game
	--  spawn_mobs()
	-- end

	-- function draw()
	--     _drw()


	-- end
	-- function update()
	--     _upd()
	--     -- update_anims()
	-- end

	cur_stt=nil
	function _TIC()
		if keyp(DBG_KEY) then dbg:toggle() end

		cur_stt=states:peek()
		-- trace(tostr(cur_stt).."|"..tostr(cur_stt.upd).."|"..tostr(cur_stt.drw))
		cur_stt.upd()
		cur_stt.drw()

		-- local str = ""
		-- for i,v in ipairs(states) do
		-- 	str = str .. v.name .. " "
		-- end
		-- trace(str)

		-- monitor("t1:       ", tostr(t1))
		-- monitor("dt:       ", tostr(dt))
		-- monitor("state:    ", cur_stt.name)
		dbg:draw()
	end


	init()
--=--=--=--=--=--=--=--=--=--=--=--=--=--=--=--=--=--=--=--
-- <TILES>
-- 000:000000000000000000000000000e00000000e000000000000000000000000000
-- 001:000000000000000000000000000f000000000000000000000000000000000000
-- 008:dd0dd0d000000000d0dd0dd000000000dd0dd0d000000000d0dd0dd000000000
-- 009:00d0dd000ddd0dd0d0dddd00ddd0ddd00d000000000dd00000dddd0000000000
-- 010:00dd0d000dd0ddd0d0dddd00d0dd0dd00d000000000dd00000dddd0000000000
-- 013:00dddd000dd0ddd0ddddd0dddd0ddd0d0d0000dd0d0dd00d000dd00000dddd00
-- 024:d0ddd0d000000000ddd0ddd000000000d0ddd0d000000000ddd0ddd000000000
-- 028:00000000000000000000000000000000000000d000000ddd000000dd0000dd00
-- 029:00000000000000000000000000000000dd0000d00dd00dddddd0d0d0d000000d
-- 030:00000000000000000000000000000000dd0000000dd00000d000000000dd0000
-- 041:00000000000000000000000000000000000000d000000ddd000000dd0000dd00
-- 042:00dd0d000dd0ddd0d0dd00dd000dd000dd0000d00dd00dddddd0d0ddd000000d
-- 043:00000000000000000000000000000000dd0000000dd0000000000000d0dd0000
-- 044:00000ddd000000000000000d00000000000000d000000ddd000000dd0000dd00
-- 045:00d0dd000ddd0dd00d0dd0d0000dd000dd0000d00dd00dddddd0d0ddd000000d
-- 046:ddd0000000000000d000000000000000dd0000000dd0000000000000d0dd0000
-- 057:00000ddd000000000000000d000000d000000000000000000000000000000000
-- 058:00d0dd000ddd0dd000ddd000dd00d0dd0ddd0dd000000000000dd00000dddd00
-- 059:ddd0000000000000d00000000d00000000000000000000000000000000000000
-- 060:00000ddd000000000000000d000000d000000000000000000000000000000000
-- 061:00d0dd000ddd0dd000ddd000dd00d0dd0ddd0dd000000000000dd00000dddd00
-- 062:ddd0000000000000d00000000d00000000000000000000000000000000000000
-- 073:0000000000000000000000000000000000000060000006660000006600006600
-- 074:0077070007707770707700770007700066000060066006666660606660000006
-- 075:0000000000000000000000000000000066000000066000000000000060660000
-- 076:00000000000000000000000000000000000000d000000ddd000000dd0000dd00
-- 077:00000000000000000000000000000000dd0000d00dd00dddddd0d0d0d000000d
-- 078:00000000000000000000000000000000dd0000000dd00000d000000000dd0000
-- 089:0000066600000000000000060000006000000000000000000000000000000000
-- 090:0050550005550550005550005500505505550550000000000005500000555500
-- 091:6660000000000000600000000600000000000000000000000000000000000000
-- 092:00000ddd000000000000000d0000000000000060000006660000006600006600
-- 093:0070770007770770070770700007700066000060066006666660606660000006
-- 094:ddd0000000000000d00000000000000066000000066000000000000060660000
-- 096:000000000000d000000ddd00000d0d000d0dddd0dd000000dd0ddd0d00d00d00
-- 097:0000d000000ddd00000d0d00000dddd000d000000dd0dd000dd0dd00000dd000
-- 098:000000000000d000000ddd00000d0d00000dddd00dd000000dd0dd0000d00d00
-- 099:0000d000000ddd00000d0d00000dddd00d000000dd0ddd00dd0ddd0d000dd000
-- 105:0000000000000000000000000000000000000066000006660000666600006666
-- 106:0077770007777770777777777777777766777766666006666666666666666666
-- 107:0000000000000000000000000000000066000000666000006666000066660000
-- 108:0000066600000000000000060000006000000000000000000000000000000000
-- 109:0050550005550550005550005500505505550550000000000005500000555500
-- 110:6660000000000000600000000600000000000000000000000000000000000000
-- 112:000000000d0d00000dddd0d000d0ddd00d0dddd0dd000000dd0ddd0d00d00d00
-- 113:0d0d00000dddd0d000d0ddd0000dddd000d000000dd0dd000dd0dd00000dd000
-- 114:000000000d0d00000dddd0d000d0ddd0000dddd00dd000000dd0dd0000d00d00
-- 115:0d0d00000dddd0d000d0ddd0000dddd00d000000dd0ddd00dd0ddd0d000dd000
-- 116:0000000000d00000000dddd0000d0ddd0d0ddd0ddd000000dd0ddd0d00d00d00
-- 117:00d00000000dddd0000d0ddd000ddd0d00d000000dd0dd000dd0dd00000dd000
-- 118:0000000000d00000000dddd0000d0ddd000ddd0d0dd000000dd0dd0000d00d00
-- 119:00d00000000dddd0000d0ddd000ddd0d0d000000dd0ddd00dd0ddd0d000dd000
-- 121:0000066600000000000000060000006600000000000000000000000000000000
-- 122:6655556605555550555555555555555505555550000000000005500000555500
-- 123:6660000000000000600000006600000000000000000000000000000000000000
-- 124:00000000000000000000000000000000000000dd00000ddd0000dddd0000dddd
-- 125:00000000000000000000000000000000dd0000ddddd00ddddddddddddddddddd
-- 126:00000000000000000000000000000000dd000000ddd00000dddd0000dddd0000
-- 128:00000000000d000000ddddd0000d0dd00d0dddd0dd000000dd0ddd0d00d00d00
-- 129:000d000000ddddd0000d0dd0000dddd000d000000dd0dd000dd0dd00000dd000
-- 130:00000000000d000000ddddd0000d0dd0000dddd00dd000000dd0dd0000d00d00
-- 131:000d000000ddddd0000d0dd0000dddd00d000000dd0ddd00dd0ddd0d000dd000
-- 132:0000000000d0d000000ddddd000d0ddd0d0dddd0dd000000dd0ddd0d00d00d00
-- 133:00d0d000000ddddd000d0ddd000dddd000d000000dd0dd000dd0dd00000dd000
-- 134:0000000000d0d000000ddddd000d0ddd000dddd00dd000000dd0dd0000d00d00
-- 135:00d0d000000ddddd000d0ddd000dddd00d000000dd0ddd00dd0ddd0d000dd000
-- 140:00000ddd000000000000000d000000dd00000066000006660000666600006666
-- 141:dd7777dd07777770777777777777777766777766666006666666666666666666
-- 142:ddd0000000000000d0000000dd00000066000000666000006666000066660000
-- 144:00000000000dd0000dddddd0000d0d000d0dd000dd000000dd0ddd0d00d00d00
-- 145:000dd0000dddddd0000d0d00000dd00000d000000dd0dd000dd0dd00000dd000
-- 146:00000000000dd0000dddddd0000d0d00000dd0000dd000000dd0dd0000d00d00
-- 147:000dd0000dddddd0000d0d00000dd0000d000000dd0ddd00dd0ddd0d000dd000
-- 148:0000000000d0d000000ddddd000d0ddd0d0ddddddd000000dd0ddd0d00d00d00
-- 149:00d0d000000ddddd000d0ddd000ddddd00d000000dd0dd000dd0dd00000dd000
-- 150:0000000000d0d000000ddddd000d0ddd000ddddd0dd000000dd0dd0000d00d00
-- 151:00d0d000000ddddd000d0ddd000ddddd0d000000dd0ddd00dd0ddd0d000dd000
-- 156:0000066600000000000000060000006600000000000000000000000000000000
-- 157:6655556605555550555555555555555505555550000000000005500000555500
-- 158:6660000000000000600000006600000000000000000000000000000000000000
-- 160:00000000000d000000dddd00000d0dd00d0dddd0dd000000dd0ddd0d00d00d00
-- 161:000d000000dddd00000d0dd0000dddd000d000000dd0dd000dd0dd00000dd000
-- 162:00000000000d000000dddd00000d0dd0000dddd00dd000000dd0dd0000d00d00
-- 163:000d000000dddd00000d0dd0000dddd00d000000dd0ddd00dd0ddd0d000dd000
-- 164:0000000000d0d000000ddd00000d0dd00d0ddddddd000000dd0ddd0d00d00d00
-- 165:00d0d000000ddd00000d0dd0000ddddd00d000000dd0dd000dd0dd00000dd000
-- 166:0000000000d0d000000ddd00000d0dd0000ddddd0dd000000dd0dd0000d00d00
-- 167:00d0d000000ddd00000d0dd0000ddddd0d000000dd0ddd00dd0ddd0d000dd000
-- 176:0d00000000ddd00000dddd0000dd0dd00d0ddd00dd000000dd0ddd0d00d00d00
-- 177:0dddd00000dddd0000dd0dd0000ddd0000d000000dd0dd000dd0dd00000dd000
-- 178:000000000dddd00000dddd0000dd0dd0000ddd000dd000000dd0dd0000d00d00
-- 179:0dddd00000dddd0000dd0dd0000ddd000d000000dd0ddd00dd0ddd0d000dd000
-- 180:0000000000d0d000000ddd00000d0dd00d0dd0dddd000000dd0ddd0d00d00d00
-- 181:00d0d000000ddd00000d0dd0000dd0dd00d000000dd0dd000dd0dd00000dd000
-- 182:0000000000d0d000000ddd00000d0dd0000dd0dd0dd000000dd0dd0000d00d00
-- 183:00d0d000000ddd00000d0dd0000dd0dd0d000000dd0ddd00dd0ddd0d000dd000
-- 192:00000000000dd00000dddd00000d0d000d0dddd0dd000000dd0ddd0d00d00d00
-- 193:000dd00000dddd00000d0d00000dddd000d000000dd0dd000dd0dd00000dd000
-- 194:00000000000dd00000dddd00000d0d00000dddd00dd000000dd0dd0000d00d00
-- 195:000dd00000dddd00000d0d00000dddd00d000000dd0ddd00dd0ddd0d000dd000
-- 196:00000000000d000000dddd00000d0ddd0d0ddd0ddd000000dd0ddd0d00d00d00
-- 197:000d000000dddd00000d0ddd000ddd0d00d000000dd0dd000dd0dd00000dd000
-- 198:00000000000d000000dddd00000d0ddd000ddd0d0dd000000dd0dd0000d00d00
-- 199:000d000000dddd00000d0ddd000ddd0d0d000000dd0ddd00dd0ddd0d000dd000
-- 200:4444044444444044400000044040440400000004404404004000000444444444
-- 201:0444444044044444400000044044040040000004404044044000000444044444
-- 202:4444440040000040404440044000004044444400000000000044000000440000
-- 206:4400000044000000440440004404400000044040440000404404404000000000
-- 207:4000000040440000404404400044044040000440404400004044044000000000
-- 208:00000000000d0d00000ddd00000d0dd0d0ddddd00ddddd000ddddd000d0d0d00
-- 209:00000000000d0d00000ddd00000d0dd000ddddd0dddddd000ddddd000d0d0d00
-- 210:00000000000d0d00000ddd00000d0dd000ddddd00ddddd00dddddd000d0d0d00
-- 211:00000000000d0d00000ddd00000dddd000ddddd0dddddd000ddddd000d0d0d00
-- 212:00000000000dd00000dddd00000d0dd00d0ddd00dd000000dd0ddd0d00d00d00
-- 213:000dd00000dddd00000d0dd0000ddd0000d000000dd0dd000dd0dd00000dd000
-- 214:00000000000dd00000dddd00000d0dd0000ddd000dd000000dd0dd0000d00d00
-- 215:000dd00000dddd00000d0dd0000ddd000d000000dd0ddd00dd0ddd0d000dd000
-- 224:00000000000000000000000000ddd0000ddd0d00ddddd0d0ddddddd00ddddd00
-- 225:0000000000ddd0000ddd0d000ddd0d000ddddd000ddddd0000ddd00000000000
-- 226:00000000000000000000000000ddd0000ddd0d00ddddd0d0ddddddd00ddddd00
-- 227:000000000000000000000000000000000dddddd0ddddd00ddddddddd0dddddd0
-- 228:00000000000ddd00000dddd0000d0d000d0dd000dd000000dd0ddd0d00d00d00
-- 229:000ddd00000dddd0000d0d00000dd00000d000000dd0dd000dd0dd00000dd000
-- 230:00000000000ddd00000dddd0000d0d00000dd0000dd000000dd0dd0000d00d00
-- 231:000ddd00000dddd0000d0d00000dd0000d000000dd0ddd00dd0ddd0d000dd000
-- 232:4400000044000000440440004404400000044040440000404404404000000000
-- 233:4044404000000000404440400044400040444040000440004044404000000000
-- 234:0444440040000040404440400044400040444040400440404044404000000000
-- 235:0044400004000400400400404044404000444000400440404044404000000000
-- 236:0000000000444400040000400404404004444440000000000444444000000000
-- 237:0044400040444040400000404004004044404440000000004444444000000000
-- 238:0000000000444000040004000044400004004400044444000044400000000000
-- 239:0044400004000400040004004044404044004440044444000044400000000000
-- 240:00000000000d0d0000dddd00000d0ddd0d0ddddddd000000dd0ddd0d00d00d00
-- 241:000d0d0000dddd00000d0ddd000ddddd00d000000dd0dd000dd0dd00000dd000
-- 242:00000000000d0d0000dddd00000d0ddd000ddddd0dd000000dd0dd0000d00d00
-- 243:000d0d0000dddd00000d0ddd000ddddd0d000000dd0ddd00dd0ddd0d000dd000
-- 244:00000000000000000000ddd0000dd0d00d0ddd00dd000000dd0ddd0d00d00d00
-- 245:000000000000ddd0000dd0d0000ddd0000d000000dd0dd000dd0dd00000dd000
-- 246:00000000000000000000ddd0000dd0d0000ddd000dd000000dd0dd0000d00d00
-- 247:000000000000ddd0000dd0d0000ddd000d000000dd0ddd00dd0ddd0d000dd000
-- 248:ffffff00000000f0ff0000f0ff0000f0ff0ff0f0ff0ff0f0ff0ff0f000000000
-- 249:f0fff0f000000000f00000f000000000f00000f000000000f00000f000000000
-- 250:0fffff00f00000f0f00000f000000000f00000f0f00000f0f00000f000000000
-- 251:00fff0000f000f00f00000f0f00000f000000000f00000f0f00000f000000000
-- 252:000000000ffffff00f0000f00f0000f00ffffff0000000000ffffff000000000
-- 253:00000000fffffff0f00000f0f00000f0fffffff000000000fffffff000000000
-- 254:000000000000000000000000000000000000f00000f0ff000f0ff0f000000000
-- 255:0000000000000000000000000000000000f0f0000fff0f0000fff0f000000000
-- </TILES>

-- <SPRITES>
-- 000:111111111ddddd11dd0d0dd1ddd0ddd1dd0d0dd11ddddd111111111111111111
-- 001:0d0000000dd000000ddd00000dd000000d000000000000000000000000000000
-- 016:1000001100000001000000010000000100000001000000011000001111111111
-- 017:0c0000000dc000000dde00000de000000e000000000000000000000000000000
-- 048:0000000000cc00000c00c0000c00c00000cccccc0000c0000000c0000000c000
-- 049:00000000000000000000000000000000cccccccc000000000000000000000000
-- 050:000000000000cc00000c00c0000c00c0cccccc00000c0000000c0000000c0000
-- 064:0000c0000000c0000000c0000000c0000000c0000000c0000000c0000000c000
-- 065:000000000ccccc00000c0000000c0000000c0000000c0000000c000000000000
-- 066:000c0000000c0000000c0000000c0000000c0000000c0000000c0000000c0000
-- 072:00ccc0000c000c00c00c00c0c0c0c0c0c00ccccc0c00000000cccccc0000c0c0
-- 073:00000000000000000000000000000000cccccccc00000000cccccccc00000000
-- 074:00000000000000000000000000000000cccccccc00000000cccccccc00000000
-- 075:00000000000000000000000000000000cccccccc00000000cccccccc00000000
-- 076:000ccc0000c000c00c00c00c0c0c0c0cccccc00c000000c0cccccc000c0c0000
-- 080:0000c0000000c0000000c00000cccccc0c00c0000c00c00000cc000000000000
-- 081:000000000000000000000000cccccccc00000000000000000000000000000000
-- 082:000c0000000c0000000c0000cccccc00000c00c0000c00c00000cc0000000000
-- 088:0000c0c00000c0c0000c0c00000c0c00000c0c00000c0c000000c0c00000c0c0
-- 092:0c0c00000c0c00000c0c00000c0c00000c0c00000c0c00000c0c00000c0c0000
-- 104:0000c0c00000c0c00000c0c00000c0c00000c0c00000c0c00000c0c00000c0c0
-- 108:0c0c00000c0c00000c0c00000c0c00000c0c00000c0c00000c0c00000c0c0000
-- 120:0000c0c00000c0c000000c0c00000c0c00000c0c00000c0c0000c0c00000c0c0
-- 124:0c0c00000c0c00000c0c00000c0c00000c0c00000c0c00000c0c00000c0c0000
-- 136:0000c0c000cccccc0c000000c00cccccc0c0c0c0c00c00c00c000c0000ccc000
-- 137:00000000cccccccc00000000cccccccc00000000000000000000000000000000
-- 138:00000000cccccccc00000000cccccccc00000000000000000000000000000000
-- 139:00000000cccccccc00000000cccccccc00000000000000000000000000000000
-- 140:0c0c0000cccccc00000000c0ccccc00c0c0c0c0c0c00c00c00c000c0000ccc00
-- 192:000000000810000008120000081230000812340008f7650008f7600008f70000
-- 193:000000000810000008120000081230000812340008f7650008f7600008f70000
-- 194:000000000812340008f76500089ab00008fedc00000000000000000000000000
-- 208:0800000008900000089a0000089ab00008fedc0008fed00008fe000008f00000
-- 209:0800000008900000089a0000089ab00008fedc0008fed00008fe000008f00000
-- 224:00000000810000001200000023000000340000006500000076000000f7000000
-- 225:08000000890000009a000000ab000000dc000000ed000000fe0000000f000000
-- 240:00000000000000000000000000000000000000000f1234000000050000000000
-- </SPRITES>

-- <MAP>
-- 000:8080808080808080808080808080808080808080808080808080808080809090a090a080808080809090a090a080808080a09090a08080808080a090808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080
-- 001:801010101010101010108010101080ee100e8080feee101010100eeefe8090101010908010101080909090909080101080109090908010101080a0a0808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080
-- 002:80108c109c1080808010801010108010101080fe108010800e80108010de9010a0a0a0801010108010909090108010101010101090801010108090a0808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080
-- 003:80101010101080101010809e8080809e8080801010101010101010101080a01010a01080801080801090909090808080801010101080801080801090808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080
-- 004:801010101010800e10108010101010101010be101010101010101010108ea09010101010101010101010109090909090101010101010101010101090808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080
-- 005:8010101010ac8080108080100e1010101010be1010100e101010101010ee8080808010901010101010101010909090909010901010101010101090a0808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080
-- 006:808f8010801080801010809e8080809e8080801010101010101010101080801010801010101010a01010101010101010909090909010101010901090808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080
-- 007:808080ae808080800e10801010108010101080feee80108010801080eede8010101010101090a01010101010101010101010909090901010109010a0808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080
-- 008:80100e0e0e10101010ee80101010801010ce8080feeeee1010100eeefe80801010801010109090101010101010101010101010101090101010909090808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080
-- 009:80100e0e0e108080808080808080808080808080808080808080808080808080808010109090101010108080808080101010101010101010909090a0808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080
-- 010:80100e0e0e1010100e1010101010101010ee801010101010101010100e80a01010101010101010101010101010108080808080101010109010901090808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080
-- 011:801010101010808080808080801080801010ae1010101010101010101080a0101010101010101010a010801010108010101080101010109010101090808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080
-- 012:808080ae808080808080808080ae8080808080809e8080101080809e80809090101080801080808010108010101010101010801010109090101090a0808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080
-- 013:8080101010801010101080101010100e10ce8010108010100e108010108090901010801010101080101080101010808010808010101090101010a090808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080
-- 014:8080ee10fe80101010ee10101010101010ce80101080100e10108010108090a09010801010101080101080808080801010101010109090101090a090808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080
-- 015:8080eefefe80fe10eeee801010eefefe10ce801010808010108080101080909090908010101010801010101010101010101010109010101090a09090808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080
-- 016:808080808080808080808080808080808080808080808080808080808080909090908080808080809090a090a0909090a09090a0909090a09090a090808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080
-- 017:808080808080808080808080808080808080808080808080808080808000808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080
-- 018:8080808080801010ce8010101080ee100e8080feee101010100eeefe80008080808f10108c80808080101010100e80101010eefefe80101010108e80808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080
-- 019:80808f101080101010801010108010101080fe108010800e80108010de0080808010101010be1010101010ce10108010101010101080100e10101080808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080
-- 020:8080101010809e8080809e8080809e80808010101010101010101010800080808010101010808080801010101010ae100e1010101080101010101080808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080
-- 021:8080101010be1010101010101010101010be101010101010101010108e00808080101010ee8080808010101010ee801010101010ee80100e10100e80808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080
-- 022:8080ee1010be10101010100e1010101010be1010100e101010101010ee00808080101010fe8080808080ae8080808080808080808080101010ce1080808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080
-- 023:8080ee1010809e8080809e8080809e80808010101010101010101010800080808080be8080808010100e100e101010fefe8010101080101010101080808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080
-- 024:808080ae8080100e10801010108010101080feee80108010801080eede0080fe1010101010fe8010101010101010108080801010108080809e808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080
-- 025:80eefe1010801010ee80101010801010ce8080feeeee1010100eeefe80008010808010808010be1010101010ac1010ae101010101080808010808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080
-- 026:80ee1010108080808080808080808080808080808080808080808080800080108010101080108010eeee10101010fe80101010101080808010808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080
-- 027:801010101010100e1010101010101010ee801010101010101010100e800080ce1010801010ce808080808080808080808080809e8080808010808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080
-- 028:80101010fe808080808080801080801010ae10101010101010101010800080108010101080108080808080feee80eefe801010101010808010808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080
-- 029:8080ae808080808080808080ae8080808080809e8080101080809e8080008010808010808010be101010ae10101010108010109c100e9e1010808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080
-- 030:80101010801010101080101010100e10ce8010108010100e10801010800080fe1010101010fe80808080801010100e1080100e101010808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080
-- 031:80ee10fe80101010ee10101010101010ce80101080100e10108010108000808080808080808080808080801010101010801010101010808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080
-- 032:80eefefe80fe10eeee801010eefefe10ce8010108080101080801010800080808080808080808080808080feee80eefe80fe10de10ee808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080
-- 033:808080808080808080808080808080808080808080808080808080808000808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080
-- 034:808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080
-- 035:808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080
-- 036:808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080
-- 037:808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080
-- 038:808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080
-- 039:808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080
-- 040:808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080
-- 041:808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080
-- 042:808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080
-- 043:808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080
-- 044:808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080
-- 045:808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080
-- 046:808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080
-- 047:808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080
-- 048:808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080
-- 049:808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080
-- 050:808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080
-- 051:808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080
-- 052:808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080
-- 053:808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080
-- 054:808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080
-- 055:808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080
-- 056:808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080
-- 057:808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080
-- 058:808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080
-- 059:808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080
-- 060:808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080
-- 061:808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080
-- 062:808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080
-- 063:808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080
-- 064:808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080
-- 065:808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080
-- 066:808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080
-- 067:808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080
-- 068:808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080
-- 069:808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080
-- 070:808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080
-- 071:808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080
-- 072:808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080
-- 073:808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080
-- 074:808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080
-- 075:808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080
-- 076:808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080
-- 077:808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080
-- 078:808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080
-- 079:808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080
-- 080:808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080
-- 081:808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080
-- 082:808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080
-- 083:808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080
-- 084:808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080
-- 085:808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080
-- 086:808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080
-- 087:808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080
-- 088:808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080
-- 089:808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080
-- 090:808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080
-- 091:808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080
-- 092:808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080
-- 093:808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080
-- 094:808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080
-- 095:808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080
-- 096:808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080
-- 097:808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080
-- 098:808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080
-- 099:808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080
-- 100:808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080
-- 101:808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080
-- 102:808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080
-- 103:808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080
-- 104:808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080
-- 105:808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080
-- 106:808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080
-- 107:808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080
-- 108:808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080
-- 109:808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080
-- 110:808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080
-- 111:808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080
-- 112:808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080
-- 113:808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080
-- 114:808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080
-- 115:808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080
-- 116:808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080
-- 117:808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080
-- 118:808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080
-- 119:808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080
-- 120:808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080
-- 121:808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080
-- 122:808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080
-- 123:808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080
-- 124:808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080
-- 125:808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080
-- 126:808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080
-- 127:808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080
-- 128:808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080
-- 129:808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080
-- 130:808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080
-- 131:808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080
-- 132:808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080
-- 133:808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080
-- 134:808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080
-- 135:808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080808080
-- </MAP>

-- <WAVES>
-- 000:00000000ffffffff00000000ffffffff
-- 001:0123456789abcdeffedcba9876543210
-- 002:0123456789abcdef0123456789abcdef
-- </WAVES>

-- <FLAGS>
-- 000:00000000000000005050501010500000000000000000000000101000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000007070300000000000000000000000000000000000000000000000000000000000207070703030303000000000000000000000000000000000
-- </FLAGS>

-- <PALETTE>
-- 000:1a1c2c5d275db13e53ef7d57ffd665a7f07038b76425717929366f3b5dc941a6f673eff7f4f4f4cec6c6897d7d554c4c
-- </PALETTE>

