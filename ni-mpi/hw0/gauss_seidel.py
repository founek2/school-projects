from numpy import array, zeros, diag, diagflat, dot, tril
from numpy.linalg import inv, norm
import numpy as np
from matplotlib import pyplot
import warnings

# handle warnging "RuntimeWarning: overflow encountered in matmul" as exceptions
warnings.filterwarnings("error")


def show(name, matrix):
    print(f"{name}:")
    print(matrix)


def calc_error(A, x, b):
    # Frobenius norm is default
    return norm(A @ x - b) / norm(b)


def show_plot(errors):
    pyplot.plot(errors)
    pyplot.title("Gauss-Siedel method")
    pyplot.xlabel("iteration")
    pyplot.ylabel("relative error")
    pyplot.show()


def gauss_seidel_W(A, DLinv):
    return np.eye(A.shape[0]) - DLinv @ A


def gauss_seidel(A, b, x=None, max_iterations=10000, tolerance=10**-6):
    """Solves the equation Ax=b via the Gauss seidel iterative method."""
    # Create an initial guess if needed
    if x is None:
        x = zeros(len(A[0]))

    # Prepare matrixes A = L + D + U
    D = diagflat(diag(A))
    L = tril(A, -1)
    U = A - L - D

    DLinv = inv(D + L)

    errors = np.array([])
    # Iterate for N times
    for i in range(max_iterations):
        try:
            x = DLinv @ dot(-U, x) + DLinv @ b

            errors = np.append(errors, calc_error(A, x, b))
            if calc_error(A, x, b) < tolerance:
                break
        except RuntimeWarning:
            x = np.full((A.shape[0]), np.nan)
            print("overflow")
            break

    show_plot(errors)
    return x, i, gauss_seidel_W(A, DLinv)
