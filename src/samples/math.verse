
solveTowersOfHanoi(n:int, src:string, aux:string, dst:string) := if (n = 1) then "Move disk 1 from " + src + " to " + dst else { solveTowersOfHanoi(n - 1, src, dst, aux); "Move disk " + n + " from " + src + " to " + dst; solveTowersOfHanoi(n - 1, aux, src, dst);}

n := 3
src := "A"
aux := "B"
dst := "C"

solveTowersOfHanoi(n, src, aux, dst)

-- if ( for{i:int, as[i]}) then 10 else 30



--hanoi(a:int,b:int,c:int,d:int):tuple := {xs := hanoi (n-1, b, d, c); ys := hanoi (n - 1, d, c, b); zs:= (b, c); if (a = 1) then (b,c) else appendThree(xs, zs, ys)};

--hanoi(count:int, src:int, target:int, auxiliary:int) := { if (count = 1) then (src, target) else appendThree(hanoi(count - 1, source, auxiliary, target), (src, target), hanoi(count - 1, auxiliary, target, source))}

--hanoi(3, 1, 3, 2);


-- [(1,3),(1,2),(3,2),(1,3),(2,1),(2,3),(1,3)]
-- hanoi :: Int -> Int -> Int -> Int -> [(Int, Int)]
-- hanoi n source target auxiliary
--     | n == 1    = [(source, target)]
--     | otherwise = hanoi (n-1) source auxiliary target ++
--                   [(source, target)] ++
--                   hanoi (n-1) auxiliary target source