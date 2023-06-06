#!/usr/bin/env python3

import argparse
import numpy as np
import sys
import logging
from PIL import Image, UnidentifiedImageError
from src.filters import actions
from src.helpers.CustomAction import CustomAction
from src.helpers.myerror import MyError
from inspect import signature


def apply_action(name: str, params: list, image: np.array) -> np.array:
    """Apply filter action specified by name and return modified image"""
    logging.info(f"Applying filter named '{name}'")
    logging.debug(f"|-> with parameters '{params}'")
    return actions[name][0](image, *params)


def prepare_parser(parser: argparse.ArgumentParser, actions) -> argparse.ArgumentParser:
    """Add all types of arguments and parameters to parser"""
    parser.add_argument('input_image_path', type=str,
                         help="Cesta k načtení obrázku")
    parser.add_argument('output_image_path', type=str,
                         help="Cesta k uložení obrázku")

    for name, (fn, metadata) in actions.items():
        parser.add_argument(f'--{name}', action=CustomAction, help=fn.__doc__,
                            nargs=len(signature(fn).parameters) - 1, type=metadata.get("type", None))
    return parser


def read_image(file_name: str):
    """Load image from provided path and handle exceptions"""
    try:
        return np.asarray(Image.open(file_name), dtype=np.float)
    except FileNotFoundError:
        logging.error("Obrázek nebyl nalezen")
        raise MyError
    except (UnidentifiedImageError, ValueError):
        logging.error("Obrázek nelze otevřít")
        raise MyError
    except IsADirectoryError:
        logging.error("Zadali jste cestu ke složce")
        raise MyError


def save_image(image_uint8, output_path):
    """Save image to provided path and handle exceptions"""
    try:
        Image.fromarray(image_uint8).save(output_path)
    except IOError:
        logging.error("Obrázek nelze uložit")
        raise MyError
    except ValueError:
        logging.error("Obrázek nelze uložit - špatná přípona")
        raise MyError


def main():
    logging.basicConfig(format='%(levelname)s - %(message)s', level=logging.ERROR)

    parser = prepare_parser(argparse.ArgumentParser(), actions)
    args = parser.parse_args()
    logging.info(args)

    try:
        data = read_image(args.input_image_path)
    except MyError:
        sys.exit(1)

    if "ordered_args" in args:
        for action, params in args.ordered_args:
            data = apply_action(name=action, params=params, image=data)

    try:
        save_image(
            data.astype(np.uint8),
            args.output_image_path
        )
    except MyError:
        sys.exit(1)


if __name__ == "__main__":
    main()
