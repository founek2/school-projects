from pytest import fixture


@fixture
def lenna_filters():
    """filters without extra parameter and corresponding lenna picture - 'lenna_{filter}.png'"""
    return "bw", "inverse", "mirror", "rotate", "sharpen", "blur"


@fixture
def lenna_filters_complex():
    """
    Combination of filters without extra parameter and corresponding lenna picture
    img name - 'lenna_{filter1}_{filter2}_{filter3}.png'
    """
    return ("bw", "inverse", "mirror"), ("rotate", "sharpen"), ("rotate", "inverse", "rotate")


@fixture
def resource_path():
    """Path to resources"""
    return "tests/resources"


@fixture
def lenna_path(resource_path):
    """Path to original lenna image"""
    return f"{resource_path}/lenna.png"
