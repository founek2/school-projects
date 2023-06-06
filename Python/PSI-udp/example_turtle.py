import turtle 

# turtle.tracer(0, 0)
# turtle.update()

colors = ['orange', 'red', 'pink', 'yellow', 'blue', 'green']
for x in range(50):
    turtle.pencolor(colors[x % 6])
    turtle.width(x / 5 + 1)
    turtle.forward(x)
    turtle.left(20)
    

star = turtle.Turtle()
star.penup()
star.goto(-400, 0)
star.pendown()

for i in range(22):
    star.forward(50 + 90)
    star.right(144 + 90)
    


polygon = turtle.Turtle()
polygon.penup()
polygon.goto(-100, 150)
polygon.pendown()

num_sides = 6
side_length = 160
angle = 360.0 / num_sides 

for i in range(num_sides):
    polygon.forward(side_length)
    polygon.right(angle)


turtle.exitonclick()
turtle.mainloop()