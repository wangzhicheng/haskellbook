1. Example p18 

    (\xyz.xz(yz)) (\mn.m) (\p.p)
     (\x.\y.\z.xz(yz)) (\m.\n.m) (\p.p) : currying
     (\y.\z.(\m.\n.m)z(yz)) (\p.p)       : beta reduction
     (\z.(\m.\n.m)z((\p.p)z))             : beta reduction
     \z.(\n.z)((\p.p)z)                    : beta reduction
     \z.z                                    : beta reduction

2. Exercise, Combinators

    * \x.xxx       => yes
    * \xy.zx       => no, z is free variable.
    * \xyz.xy(zx)  => yes
    * \xyz.xy(zxy) => yes
    * \xy.xy(zxy)  => no, z is free variable

3. Exercise. Normal form or diverge?

    * \x.xxx         => normal form
    * (\z.zz)(\y.yy) => diverge. *omega* term
    * (\x.xxx)z      => reducible to `zzz`
    
4. Exercise. Beta reduce

    * (\abc.cba)zz(\wv.w)
         (\a.\b.\c.cba) z z (\w.\v.w)
         (\b.\c.cbz) z (\w.\v.w)
         (\c.czz) (\w.\v.w)
         (\w.\v.w) z z
         z

    * (\x.\y.xyy)(\a.a)b
         (\y.(\a.a)yy)b
         (\a.a)bb
         bb

    * (\y.y)(\x.xx)(\z.zq)
         (\x.xx) (\z.zq)
         (\z.zq) (\z.zq)
         (\z.zq) q
         qq

    * (\z.z)(\z.zz)(\z.zy)
         (\z.zz)(\z.zy)
         (\z.zy)(\z.zy)
         (\z.zy)y
         yy

    * (\x.\y.xyy)(\y.y)y
         (\y.(\z.z)yy)y
         (\z.z)yy
         yy

    * (\a.aa)(\b.ba)c
         (\b.ba)(\b.ba)c
         (\b.ba)ac
         aac

    * (\xyz.xz(yz))(\x.z)(\x.a)
         (\x.\y.\z.xz(yz)) (\x.z) (\x.a)
         (\x.\y.\m.xm(ym)) (\x.z) (\x.a)
         (\y.\m.(\x.z)m(ym)) (\x.a)
         (\m.(\x.z)m((\x.a)m))
         \m.za

