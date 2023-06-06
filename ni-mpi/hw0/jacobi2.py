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
    pyplot.title("Jacobi method")
    pyplot.xlabel("iteration")
    pyplot.ylabel("relative error")
    pyplot.show()


def jacobi_W(A, Dinv):
    return np.eye(A.shape[0]) - Dinv @ A


def jacobi(A, b, x=None, max_iterations=10000, tolerance=10**-6):
    """Solves the equation Ax=b via the Jacobi iterative method."""
    # Create an initial guess if needed
    if x is None:
        x = zeros(len(A[0]))

    # Prepare matrixes A = L + D + U
    D = diagflat(diag(A))
    L = tril(A, -1)
    U = A - L - D

    Dinv = inv(D)

    errors = np.array([])
    # Iterate for N times
    for i in range(max_iterations):
        try:
            x = Dinv @ dot(-L-U, x) + Dinv @ b

            errors = np.append(errors, calc_error(A, x, b))
            if calc_error(A, x, b) < tolerance:
                break

        except RuntimeWarning:
            x = np.full((A.shape[0]), np.nan)
            print("overflow")
            break

    show_plot(errors)
    return x, i, jacobi_W(A, Dinv)


if __name__ == "__main__":
    n = 20
    γ = 2
    A = np.eye(20) * γ + np.eye(20, k=-1) * -1 + np.eye(20, k=1) * -1
    b = np.full((n), γ - 1) + \
        np.concatenate((array([0]), np.full((18), -1), array([0])))

    sol = jacobi(A, b, N=1000)

    # Float64 is default on modern machines
    show("x", sol)
    assert sol.dtype == np.float64
