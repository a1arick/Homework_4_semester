factorial x = if x <= 1 then 1 else x * factorial (x - 1)

main = print (factorial 6) 