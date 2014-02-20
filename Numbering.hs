module Numbering where
import URM
piF m n = 2^m*(2*n+1)-1
xiF m n q = piF (piF (m-1) (n-1)) (q-1)
tauF as = (sum $ map (2^) as)-1
tauF' x = f (x+1) 0 where 
	f 0 _ = []
	f b k	| m==0 = r
		| m==1 = k:r
		where 
		r = f d (k+1)
		(d,m) = b `divMod` 2
betaF (Z n) = 4*(n-1)
betaF (S n) = 4*(n-1)+1
betaF (T m n) = 4*(piF (m-1) (n-1))+2
betaF (J m n q) = 4*(xiF m n q)+3
betaF' x = s r where
	s 0 = Z (u+1)
	s 1 = S (u+1)
	--s 2 = T (pi1F u+1) (pi2F i+1)
	--s 3 = J m n q where (m,n,q) = xiF' u
	(u,r) = x `divMod` 4
gammaF p = tauF $ map betaF p
