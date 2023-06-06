import numpy as np
from pytest import fixture
from numpy.testing import assert_equal
from random import randrange
from src.filters import actions
from src.program import read_image


def assert_equal_uint8(data1: np.ndarray, target: np.ndarray):
    """To uint8 -> ignores float deviation"""
    assert_equal(
        data1.astype(np.uint8),  # to uint8 -> ignores float deviation
        target.astype(np.uint8))


@fixture
def grayscale():
    return np.array([x for x in range(9)]).reshape(3, 3)


@fixture
def image(lenna_path):
    return read_image(lenna_path)


@fixture
def percentage():
    return randrange(101)


def test_mirror_greyscale(grayscale):
    assert_equal(actions["mirror"][0](grayscale),
                 np.array([[2, 1, 0],
                           [5, 4, 3],
                           [8, 7, 6]]))


def test_lenna(image, lenna_filters, resource_path):
    """test all filters without extra parameter"""
    for action, (fn, metadata) in actions.items():
        if action in lenna_filters:
            assert_equal_uint8(fn(image), read_image(f"{resource_path}/lenna_{action}.png"))


def test_rotate_grayscale(grayscale):
    assert_equal(
        actions["rotate"][0](grayscale),
        np.array([[6, 3, 0],
                  [7, 4, 1],
                  [8, 5, 2]]))


def test_darken_grayscale(grayscale):
    percentage = 25
    assert_equal_uint8(
        actions["darken"][0](grayscale, percentage),
        np.array([[0., 0.75, 1.5],
                  [2.25, 3., 3.75],
                  [4.5, 5.25, 6.]]))


def test_darken_lenna(image, resource_path):
    percentage = 22
    assert_equal_uint8(
        actions["darken"][0](image, percentage),
        read_image(f"{resource_path}/lenna_darken22.png"))


def test_darken_100(grayscale):
    percentage = 100
    assert_equal_uint8(
        actions["darken"][0](grayscale, percentage),
        np.array([0 for x in range(9)]).reshape((3, 3)))


def test_darken_invalid(grayscale):
    percentage = -1
    try:
        actions["darken"][0](grayscale, percentage)
    except ValueError:
        pass


def test_darken_grayscale_random(grayscale, percentage):
    assert_equal_uint8(
        actions["darken"][0](grayscale, percentage),
        (grayscale * (100 - percentage) / 100))


def test_lighten_grayscale(grayscale):
    percentage = 25
    assert_equal_uint8(
        actions["lighten"][0](grayscale, percentage),
        np.array([[0., 1.25, 2.5],
                  [3.75, 5., 6.25],
                  [7.5, 8.75, 10.]]))


def test_lighten_lenna(image, resource_path):
    percentage = 22
    assert_equal_uint8(
        actions["lighten"][0](image, percentage),
        read_image(f"{resource_path}/lenna_lighten22.png"))


def test_lighten_grayscale_random(grayscale, percentage):
    assert_equal_uint8(
        actions["lighten"][0](grayscale, percentage),
        (grayscale * (100 + percentage) / 100))


def test_lighten_invalid(grayscale):
    percentage = 101
    try:
        actions["lighten"][0](grayscale, percentage)
    except ValueError:
        pass


def test_inverse_grayscale(grayscale):
    assert_equal(
        actions["inverse"][0](grayscale),
        np.array([[255, 254, 253],
                  [252, 251, 250],
                  [249, 248, 247]]))


def test_sharpen_grayscale(grayscale):
    assert_equal_uint8(
        actions["sharpen"][0](grayscale),
        np.array([[0, 0, 4],
                  [5, 4, 11],
                  [20, 17, 28]]))


def test_bw_invalid(grayscale):
    assert_equal(
        actions["bw"][0](grayscale),
        grayscale)
