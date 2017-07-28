forever :: IO () -> IO ()
forever a = a >> forever a
            
repeatN :: Int -> IO () -> IO ()
repeatN 0 a = return ()
repeatN n a = a >> repeatN (n-1) a


each :: [IO a] -> IO [a]
each [] = return []
each (a:as) = do
    r <- a;
    rs <- each as;
    return (r:rs)
    

