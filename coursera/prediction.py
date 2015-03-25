import pandas as pd
import numpy as np
import pylab as P

# For .read_csv, always use header=0 when you know row 0 is the header row
df = pd.read_csv('getdata.csv', header=0)

print df['FES']
