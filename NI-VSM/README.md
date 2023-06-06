# NI-VSM

```python
K = 5
L = 8 # len("skalický)
X = ((K*L*23) % (20)) + 1
X # -> 1
Y = ((X + ((K*5 + L*7) % (19))) % (20)) + 1
Y # -> 7
```
