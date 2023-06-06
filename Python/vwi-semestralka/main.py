import sys
import argparse
import pandas as pd
import numpy as np

from PySide2.QtCore import QDateTime, QTimeZone
from PySide2.QtWidgets import QApplication
from main_window import MainWindow
from main_widget import Widget


def transform_date(utc, timezone=None):
    utc_fmt = "yyyy-MM-ddTHH:mm:ss.zzzZ"
    new_date = QDateTime().fromString(utc, utc_fmt)
    if timezone:
        new_date.setTimeZone(timezone)
    return new_date


def read_data(fname):
    # Read the CSV content
    df = pd.read_csv(fname, usecols=["Id","Price","Year","Mileage","City","State","Vin","Make","Model"],
                     dtype={"Id": "Int64","Price": np.int32,"Year": np.int32,"Mileage": np.int32,"City": np.object,"State": np.object,"Vin": np.object,"Make": np.object,"Model": np.object},
                     nrows=2000)
    #df.to_csv(r'partial100k.csv', index=False)
    df.sort_values(by=['Year', "Price"], inplace=True)
    # print(df)
    #print(df.loc[(df["Year"] > 2015) & (df["Price"] > 11262)])
    #raise Exception("sds")
    return df


if __name__ == "__main__":
    options = argparse.ArgumentParser()
    options.add_argument("-f", "--file", type=str, required=True)
    args = options.parse_args()
    data = read_data(args.file)

    # Qt Application
    app = QApplication(sys.argv)

    widget = Widget(data)
    window = MainWindow(widget)
    window.show()

    sys.exit(app.exec_())
