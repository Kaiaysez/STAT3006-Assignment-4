import pandas as pd
import numpy as np
import matplotlib.pyplot as plt
from numpy import linalg as LA
import seaborn as sns

# 9th column is the label
prostate = pd.read_csv(r"C:\Users\Owner\Desktop\UQ Year 3 Sem 2 Courses"
                  r"\STAT3006\Assignment 4\Data and Question Sheet\prostate.csv")


sns.pairplot(prostate)

# Elastic net penalty plot (Unrelated to dataset)

gamma = 1

x = []
y = []
for i in np.linspace(-5,5,1000):
    for j in np.linspace(-5,5,1000):
        if LA.norm(np.array([i,j]), ord = 1) + (LA.norm(np.array([i,j]), ord = 2))**2 <= gamma:
            x.append(i)
            y.append(j)
plt.scatter(x,y,s = 1)
plt.ylim((-3,3))
plt.xlim((-3,3))
plt.show()

