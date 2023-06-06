import numpy as np
from src.helpers.convolution import apply_filter, kernels
from src.helpers.custom_types import percentage

CHANEL_WEIGHTS = [0.299, 0.587, 0.114]


def helper_clip(data: np.array) -> np.array:
    return np.clip(data, a_min=0, a_max=255)


def mirror(image: np.array) -> np.array:
    """Mirrors image"""
    return np.flip(image, axis=1)


def rotate(image: np.array) -> np.array:
    """Rotates image by 90 degree to right"""
    return np.rot90(image, 3)  # rotate 270 degree to left


def bw(image: np.array) -> np.array:
    """Transfers rgb image to greyscale - if not in rgb -> returns input image"""
    if len(image.shape) <= 2 or image.shape[-1] != 3:
        return image
    return np.average(image, weights=CHANEL_WEIGHTS, axis=2)


def darken(image: np.array, percentage: int) -> np.array:
    """Darkens image by percentage: <0-100>"""
    if percentage < 0 or percentage > 100:
        raise ValueError
    return image * (1 - percentage / 100)


def lighten(image: np.array, percentage: int) -> np.array:
    """Lightens image by percentage: <0-100>"""
    if percentage < 0 or percentage > 100:
        raise ValueError
    return helper_clip(
        image * (1 + percentage / 100)
    )


def inverse(image: np.array) -> np.array:
    """Inverts image (negative)"""
    return 255 - image


def sharpen(image: np.array) -> np.array:
    """Applies 'unsharp mask' to image"""
    return apply_filter(image, kernels["sharpening_kernel"])


def blur(image: np.array) -> np.array:
    """Applies 'gaussian blur 3x3' to image"""
    return apply_filter(image, kernels["approx_gaussian_blur_3_kernel"])


actions = {
    # (function, metadata)
    "mirror": (mirror, {}),
    "rotate": (rotate, {}),
    "bw": (bw, {}),
    "darken": (darken, {"type": percentage}),
    "lighten": (lighten, {"type": percentage}),
    "inverse": (inverse, {}),
    "sharpen": (sharpen, {}),
    "blur": (blur, {}),
}
