module L
( sep, lg, rg
, 一, 阳, 阴
, 预, 止, 同
, 缘, 是, 非
, 指, 月
, 坤
, 有, 无
, nSep, sep3, sep7
) where

-- magicChars = "'," bad syntax sugar, never use then

sep = " "
lg = '「'
rg = '」'

一 = "一"
阳 = "阳"
阴 = "阴"
预 = "预"
止 = "止"
同 = "同"

缘 = "缘"
是 = "是"
非 = "非"
指 = "指"
月 = "月"
坤 = "坤"

有 = "有"
无 = "无"

nSep n = concat (replicate n sep)
sep3 = nSep 3
sep7 = nSep 7

