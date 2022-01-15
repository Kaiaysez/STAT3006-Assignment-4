import pandas as pd
import numpy as np
import matplotlib.pyplot as plt
from sklearn.decomposition import PCA
from mpl_toolkits.axes_grid1 import AxesGrid
import seaborn as sns
from numpy import linalg as LA

df = pd.read_csv(r"C:\Users\Owner\Desktop\UQ Year 3 Sem 2 Courses"
                  r"\STAT3006\Assignment 4\Data and Question Sheet\zip.txt", sep = None, header = None)
del df[257]
y = np.array(df.iloc[:,0])
x = np.array(df.iloc[:,1:258])


"""
#############################
### PROBLEM 1 PART 1 ########
#############################
                         """

def plot_numbers(number):
    # Find 9 observations where the label is 8
    eight = []
    for i in range(len(y)):
        if y[i] == number:
            eight.append(i)
        if len(eight) >= 9:
            break
    
    # Make a list of the matrix representations of the 9 observations found
    eight_as_matrix = []
    for i in eight:
        eight_as_matrix.append(np.array(x[i]).reshape((16,16)))      
            
         
    # Make a grid to plot matrix representations
    fig = plt.figure()
    grid = AxesGrid(fig, 111,
                    nrows_ncols=(3, 3),
                    axes_pad=0.05,
                    share_all=True,
                    label_mode="L",
                    cbar_location="right",
                    cbar_mode="single",
                    )
    
    # Plot matrix representations as heatmaps with color = "viridis"
    for val, ax in zip(eight_as_matrix,grid):
        im = ax.imshow(val, vmin=0, vmax=1, cmap = "viridis")
        ax.set_axis_off()
    grid.cbar_axes[0].colorbar(im)
    plt.show()

# Do gridplots of digits 1 to 9
for i in range(10):
    plot_numbers(i)

"""
#############################
### PROBLEM 1 PART 2 ########
#############################
                          """

# Do dimensionality reduction (PCA)
pca = PCA(n_components=4)
pca.fit(x) # PCA from sklearn library centers automatically 
trans_data = pca.transform(x) # Map the data to the lower dimensional space
print("PCs are:\n",pca.components_,"\n") # These are the principal components

# Find the minimum value of the optimization problem in part 2
xbar = np.array(pd.DataFrame(x).mean()) # Mean of all the features
xtilda = x - xbar # Center the data 
O_transpose = pca.components_
O = O_transpose.T
R = O
F = O_transpose
RF = np.matmul(R,F)
minlist = []
for i in range(7291):
    minlist.append((LA.norm(xtilda[i] - np.matmul(RF,xtilda[i])))**2)
minimum = sum(minlist)
print("The minimum value is:\n",sum(minlist),"\n")


"""
#############################
### PROBLEM 1 PART 3 ########
#############################
                          """

# Plot the transformed data
trans_data = pd.DataFrame(trans_data)
trans_data['Label'] = y.astype(int)
order = [0,1,2,3,4,5,6,7,8,9]
sns.pairplot(trans_data, hue = 'Label', vars = [0,1,2,3], 
             palette = "tab10", hue_order = order)



"""
#############################
### PROBLEM 1 PART 4 ########
#############################
                          """
                          
print("Variance proportion explained by each PC:\n",pca.explained_variance_ratio_,"\n")
# print("Variance (4 largest eigenvalues of the covariance matrix) explained by" 
#      r" each PC:","\n", pca.explained_variance_,"\n")
#print("Sum of the above eigenvalues:\n",sum(pca.explained_variance_),"\n")
# Verifying that Sklearn pc is solving the same problem as in the lecture notes
# LA.norm(pca.components_[0]) # Verifying that PC's are unit length
# G = (1/7291)*np.matmul(xtilda.T,xtilda) # Calculate the covariance matrix
# GO = np.matmul(G,O)
# O_transpose_GO = np.matmul(O_transpose,GO) 
# print("A manual calculation yields the same answer:\n",
#       np.trace(O_transpose_GO)) # Calculate the trace of the matrix (O.T)(G)(O) 


"""
#############################
### PROBLEM 1 PART 5 ########
#############################
                          """
                
#p = ProbabilisticPCA()