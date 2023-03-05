function twice(x)
    y = (x * 2)
    return y
end

function square(a)
    return (a * a)
end

a = 5
print("square area is", square(a))
print("and its perimeter is", twice(twice(a)))
