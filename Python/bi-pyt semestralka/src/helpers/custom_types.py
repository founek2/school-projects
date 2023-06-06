import argparse


def percentage(val: str) -> int:
    """Checks if value is valid percentage -> if not, then raise exception"""
    ivalue = int(val)
    if ivalue < 0 or ivalue > 100:
        raise argparse.ArgumentTypeError(f"{ivalue} is an invalid percentage int value")
    return ivalue
