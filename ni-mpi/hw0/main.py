from jacobi2 import jacobi
from gauss_seidel import gauss_seidel
import numpy as np
from numpy import array, zeros, diag, diagflat, dot, tril
from numpy.linalg import inv, norm, eigvals
# import warnings

# supress warnging "RuntimeWarning: overflow encountered in matmul" (to make prettier output)
# which occours when calculating γ = 1/2 which diverges
# warnings.filterwarnings('ignore')


def spectral_radius(matrix):
    return np.max(np.abs(eigvals(matrix)))


n = 20

# for γ in [5, 2]:
for γ in [5, 2, 1/2, 3, 1, 15]:
    A = np.eye(20) * γ + np.eye(20, k=-1) * -1 + np.eye(20, k=1) * -1
    b = np.full((n), γ - 1) + \
        np.concatenate((array([0]), np.full((18), -1), array([0])))

    sol, i, W = jacobi(A, b)

    # iteration method converges <=> ρ(W) < 1
    print(spectral_radius(W))
    print(
        f"JACOBI       method: iteration_cnt={i}, ρ(W) < 1 = {spectral_radius(W) < 1}, γ={γ}")

    sol, i, W = gauss_seidel(A, b)
    print(spectral_radius(W))
    print(
        f"GAUSS-SIEDEL method: iteration_cnt={i}, ρ(W) < 1 = {spectral_radius(W) < 1}, γ={γ}")

    # Float64 is default on modern machines
    assert sol.dtype == np.float64
