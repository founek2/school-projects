import turtle

t = turtle.Turtle()
num_sides = 50
angle = 20

colors = ["red", "blue", "brown", "yellow"]

t.width(5)
for x in range(50): # 0, 1, 2...
    t.forward(x)
    t.left(20)
    t.pencolor(colors[x % len(colors)]  )

turtle.exitonclick()