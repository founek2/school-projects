import os
from pathlib import Path
from pytest import fixture

TMP = "tests/tmp"
OUT_PATH = f"{TMP}/out.png"


@fixture(autouse=True)
def run_around_tests():
    # Code that will run before your test
    tmp = Path(TMP)
    tmp.mkdir(exist_ok=True)
    yield tmp
    # Code that will run after your test
    for sub in tmp.iterdir():
        sub.unlink()
    tmp.rmdir()


def test_help():
    exit_status = os.system('python3 . --help')
    assert exit_status == 0


def test_individual_filters(lenna_filters, lenna_path, resource_path):
    for action in lenna_filters:
        exit_status = os.system(f'python3 . --{action} {lenna_path} {OUT_PATH}')
        assert exit_status == 0
        assert os.system(f"diff {resource_path}/lenna_{action}.png {OUT_PATH}") == 0


def test_complex(lenna_filters_complex, lenna_path, resource_path):
    """Test different complex combination of filters"""
    for actions in lenna_filters_complex:
        exit_status = os.system(f'python3 . --{" --".join(actions)} {lenna_path} {OUT_PATH}')
        assert exit_status == 0
        assert os.system(f"diff {resource_path}/lenna_{'_'.join(actions)}.png {OUT_PATH}") == 0


def test_preserve_order(lenna_path, resource_path):
    exit_status = os.system(f'python3 . --sharpen --bw {lenna_path} {OUT_PATH}')
    assert exit_status == 0
    assert os.system(f"diff {resource_path}/lenna_sharpen_bw.png {OUT_PATH}") == 0

    exit_status = os.system(f'python3 . --bw --sharpen {lenna_path} {OUT_PATH}')
    assert exit_status == 0
    assert os.system(f"diff {resource_path}/lenna_sharpen_bw.png {OUT_PATH}") != 0


def test_lighten(lenna_path, resource_path):
    exit_status = os.system(f'python3 . --rotate --bw --lighten 30 {lenna_path} {OUT_PATH}')
    assert exit_status == 0
    assert os.system(f"diff {resource_path}/lenna_rotate_bw_lighten30.png {OUT_PATH}") == 0


def test_darken(lenna_path, resource_path):
    exit_status = os.system(f'python3 . --inverse --sharpen --darken 30 {lenna_path} {OUT_PATH}')
    assert exit_status == 0
    assert os.system(f"diff {resource_path}/lenna_inverse_sharpen_darken30.png {OUT_PATH}") == 0


def test_invalid_input_path(lenna_path):
    exit_status = os.system(f'python3 . --inverse --sharpen --darken 30 / {OUT_PATH}')
    assert exit_status == 1 * 256  # at least on macOS returned value is multiplied by 256


def test_invalid_output_path(lenna_path):
    exit_status = os.system(f'python3 . --inverse --sharpen --darken 30 {lenna_path} /dev/kekel/sdsd.png')
    assert exit_status == 1 * 256


def test_invalid_output_path2(lenna_path):
    exit_status = os.system(f'python3 . --inverse --sharpen --darken 30 {lenna_path} /dev/kekel/sdsd')
    assert exit_status == 1 * 256


def test_invalid_params(lenna_path):
    exit_status = os.system(f'python3 . --inverse --blabla --darken 30 {lenna_path} {OUT_PATH}')
    assert exit_status == 2 * 256


def test_missing_path(lenna_path):
    exit_status = os.system(f'python3 . --inverse --darken 30 {lenna_path}')
    assert exit_status == 2 * 256
