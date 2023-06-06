import numpy as np

strd = np.lib.stride_tricks.as_strided


def apply_filter(image: np.array, kernel: np.array) -> np.array:
    # A given image has to have either 2 (grayscale) or 3 (RGB) dimensions
    assert image.ndim in [2, 3]
    # A given filter has to be 2 dimensional and square
    assert kernel.ndim == 2
    assert kernel.shape[0] == kernel.shape[1]

    # save original shape
    original_shape = image.shape

    width, height = image.shape[0:2]
    image = image.reshape(width, height, -1)  # reshape to (w,h,3|1)

    kernel_width = kernel.shape[0]
    k_pad = kernel_width // 2
    npad = ((k_pad, k_pad), (k_pad, k_pad), (0, 0))
    image_padded = np.pad(image, pad_width=npad, mode="constant")  # zero pading

    # new shape with sub matrix with shape of kernel in every color
    new_shape = image.shape + kernel.shape

    # generate new tensor, where in every color channel will be matrix with size of kernel
    # image_padded.strides[0:2] - guarantee shape of matrix in every color channel
    # image_padded.strides - guarantee to go through every pixel and every color channel
    sub_matrixes = strd(image_padded, shape=new_shape, strides=image_padded.strides + image_padded.strides[0:2])

    # multiply every sub matrix with kernel
    multiplied = sub_matrixes * kernel

    # sum whole matrix in every color channel
    summed = np.sum(multiplied, axis=(3, 4))

    # saturate to max 255 and min 0 - uint8
    final_matrix = np.clip(summed, a_min=0, a_max=255)

    # reshape to original shape (grayscale is transformed for computation to (width, height, 1)
    return final_matrix.reshape(original_shape)


kernels = {
    "identity_kernel": np.array([
        [0, 0, 0],
        [0, 1, 0],
        [0, 0, 0],
    ]),
    "sharpening_kernel": np.array([
        [0, -1, 0],
        [-1, 5, -1],
        [0, -1, 0],
    ]),
    "approx_gaussian_blur_3_kernel": (1 / 16) * np.array([
        [1, 2, 1],
        [2, 4, 2],
        [1, 2, 1],
    ]),
    "approx_gaussian_blur_5_kernel": (1 / 256) * np.array([
        [1, 4, 6, 4, 1],
        [4, 16, 24, 16, 4],
        [6, 24, 36, 24, 6],
        [4, 16, 24, 16, 4],
        [1, 4, 6, 4, 1],
    ]),
    "edge_detection_kernel": np.array([
        [-1, -1, -1],
        [-1, 8, -1],
        [-1, -1, -1],
    ]),
    "embossing_kernel": np.array([
        [-2, -1, 0],
        [-1, 1, 1],
        [0, 1, 2]
    ])
}
