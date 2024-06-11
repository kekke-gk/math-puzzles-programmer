count w h = (w - 1) + (h - 1) + 3 * ((w - 1) + (h - 2))

main = do
    print $ count 10 10
