import numpy as np
import matplotlib.pyplot as plt
import matplotlib.cm as cm
import seaborn as sns
import pandas as pd

def getAveragedMetrics(path1, path2, g, tmax, mode):
    accuracies=[]
    timesList = []
    costs=[]
    for k in np.arange(0.001,1.05,0.05):
        suffix ="_k_"+str(k)[0]+"_"+str(k)[2]+"_"+str(k)[3]+"_za_0_0_0.txt"
        files = ["a_wins_points", "b_wins_points", "deadlock_points", "convergence_time", "convergence_times"]
        dataMoreA = []
        for i in range(len(files)):
            f = open(path1 + files[i]+suffix, mode="r", encoding="utf-8")
            dataMoreA.append(f.readlines())
            f.close()
        dataMoreB = []
        for i in range(len(files)):
            f = open(path2 + files[i] + suffix, mode="r", encoding="utf-8")
            dataMoreB.append(f.readlines())
            f.close()
        #print(len(dataMoreA[0]))
        #print(len(dataMoreA[1]))
        #print(len(dataMoreA[2]))
        accuracyMoreA = len(dataMoreA[0])/(sum([len(dataMoreA[0]), len(dataMoreA[1]), len(dataMoreA[2])]))
        accuracyMoreB = len(dataMoreB[0]) / (sum([len(dataMoreB[0]), len(dataMoreB[1]), len(dataMoreB[2])]))
        accuracy = (accuracyMoreA+accuracyMoreB)/2
        accuracies.append(accuracy)

        convergenceTimesListMoreA = []
        for i in range(len(dataMoreA[4])):
            time = float(dataMoreA[4][i].replace("{", "").replace("}", "").split(",")[2])
            if mode=="d":
                if time>=tmax:
                    convergenceTimesListMoreA.append(tmax)
                else:
                    convergenceTimesListMoreA.append(time)
            elif time<tmax:
                convergenceTimesListMoreA.append(time)

        convergenceTimesListMoreB = []
        for i in range(len(dataMoreB[4])):
            time = float(dataMoreB[4][i].replace("{", "").replace("}", "").split(",")[2])
            if mode=="d":
                if time>=tmax:
                    convergenceTimesListMoreB.append(tmax)
                else:
                    convergenceTimesListMoreB.append(time)
            elif time<tmax:
                convergenceTimesListMoreB.append(time)

        avgTimeA = sum([float(x) for x in convergenceTimesListMoreA])/len(convergenceTimesListMoreA)
        avgTimeB = sum([float(x) for x in convergenceTimesListMoreB])/len(convergenceTimesListMoreB)
        avgTime = (avgTimeA+avgTimeB)/2

        timesList.append(avgTime)

        cost = avgTime * (k + (1-k)*g)

        costs.append(cost)

    return [accuracies, timesList, costs]

def getMetrics(path1, g, tmax, mode, zName):
    accuracies=[]
    regrets=[]
    avgTimes = []
    costs=[]
    timesList = [] #1 per k
    for k in np.arange(0.001,1.05,0.05):
        if zName=="za_0":
            suffix ="_k_"+str(k)[0]+"_"+str(k)[2]+"_"+str(k)[3]+"_za_0_0_0.txt"
        else:
            suffix ="_k_"+str(k)[0]+"_"+str(k)[2]+"_"+str(k)[3]+"_za_0_0_5.txt"
        files = ["a_wins_points", "b_wins_points", "deadlock_points", "convergence_time", "convergence_times"]
        dataMoreA = []
        for i in range(len(files)):
            f = open(path1 + files[i]+suffix, mode="r", encoding="utf-8")
            dataMoreA.append(f.readlines())
            f.close()

        accuracy = len(dataMoreA[0])/(sum([len(dataMoreA[0]), len(dataMoreA[1]), len(dataMoreA[2])]))
        regret = len(dataMoreA[2])
        for point in dataMoreA[1]:
            regret+=(1-float(point.replace("{", "").replace("}", "").split(",")[1]))

        regrets.append(regret/(sum([len(dataMoreA[0]), len(dataMoreA[1]), len(dataMoreA[2])])))
        accuracies.append(accuracy)

        convergenceTimesListMoreA = []
        for i in range(len(dataMoreA[4])):
            time = float(dataMoreA[4][i].replace("{", "").replace("}", "").split(",")[2])
            if mode=="d":
                if time>=tmax:
                    convergenceTimesListMoreA.append(tmax)
                else:
                    convergenceTimesListMoreA.append(time)
            elif time<tmax:
                convergenceTimesListMoreA.append(time)


        avgTime = sum([float(x) for x in convergenceTimesListMoreA])/len(convergenceTimesListMoreA)

        avgTimes.append(avgTime)

        cost = avgTime * (k + (1-k)*g)

        costs.append(cost)

        timeList = dataMoreA[4]
        timesList.append(timeList)

    return [accuracies, avgTimes, costs, timesList, regrets]

#returns the BR and the minima of a model
def compute_BR_and_minima(name, G, tmax, zName):
    path1 = name+"/more_a/" + zName+ "/"
    metrics = getMetrics(path1, G, tmax, mode, zName)
    accuracies = metrics[0]
    times = metrics[1]
    costs = metrics[2]
    timesList = metrics[3] 
    regrets = metrics[4]
    hList=[]
    kList=[]
    brList = []
    minima=[]

    for h in np.arange(0, 1000.1, 0.1):
        minimumBR = np.infty
        minimumBRPoint = [-1,-1]
        for k in np.arange(0, 1.05, 0.05):
            #if "chci" in name:
            br = costs[int(k*20)] + h * (regrets[int(k*20)])
            #elif "chds" in name:
            #    br = costs[int(k*20)] + h * (1-accuracies[int(k*20)])
            if br<minimumBR:
                minimumBR = br
                minimumBRPoint = [round(k,2), round(h,2)]
            hList.append(round(h,2))
            kList.append(round(k,2))
            brList.append(br)
        minima.append(minimumBRPoint)
        #if minimumBRPoint[0]>=1.0:
         #   print("for k=1, h=", minimumBRPoint[1])
    print(max(brList))
    return [kList, hList, brList, minima]

#plots a heatmap with the minimum BR for each k,h 
def plotBR(name, G, tmax, zName, za, mode="AB"):
    
    path2 = name +"/more_b/" + zName+ "/"

    #metrics = getAveragedMetrics(path1, path2, G, tmax, mode)
    
    kList, hList, brList, minima = compute_BR_and_minima(name, G, tmax, zName)
    bayesianRiskMatrix = np.array(brList).reshape(10001,21)
    data = pd.DataFrame({'k': kList,'h': hList, 'br': brList})
    #sns.set()
    data_pivoted = data.pivot(columns="k", index="h", values="br")
    fig, ax = plt.subplots()
    plt.gca().set_aspect('equal')
    #fig, (cax, ax) = plt.subplots(nrows=2, figsize=(15,15.025), gridspec_kw={"height_ratios":[0.025,1]})
    #ax = sns.heatmap(data_pivoted,annot=False,cmap="viridis",linewidths=0.0, cbar=False)#, rasterized=True)#{'label': 'Bayesian risk','use_gridspec'='False','location'="top"})
    #fig.colorbar(ax.get_children()[0], cax=cax, orientation="horizontal", label="Bayesian risk")

    img = ax.imshow(bayesianRiskMatrix, cmap='viridis', aspect='auto', vmin=0, vmax=483)
    #ax2,ax3 = ax.twinx(), ax.twiny()
    ax.set_xticks(np.arange(21), labels=[str(round(x,2)) for x in np.arange(0,1.05,0.05)])
    ax.set_yticks(np.arange(0,12000, 200), labels=[str(x) for x in np.arange(0,1200, 20)])
    #ax.set_yticks(np.arange(len(vegetables)), labels=vegetables)
    '''plt.setp(ax2.get_xticklabels(), visible=False)
    plt.setp(ax2.get_yticklabels(), visible=False)
    plt.setp(ax3.get_xticklabels(), visible=False)
    plt.setp(ax3.get_yticklabels(), visible=False)'''
    #idx_min_big = data_pivoted.values.flatten().argmin()
    #x_min_big, y_min_big = (idx_min_big % (1000*21))/21, 1000-(idx_min_big//(1000*21))/1000
    
    ax.scatter([x[0]*20 for x in minima], [y[1]*10 for y in minima], s=140, c="red")

    ax.invert_yaxis()
    for ind, label in enumerate(ax.get_yticklabels()):
        if (ind % 10 == 0):  # every 10th label is kept
            label.set_visible(True)
        else:
            label.set_visible(False)
    for ind, label in enumerate(ax.get_xticklabels()):
        if (ind % 2 == 0):  # every 10th label is kept
            label.set_visible(True)
        else:
            label.set_visible(False)
    ax.tick_params(labelsize=30)
    ax.tick_params(labelsize=30)
    ax.set_xlabel(r'swarm heterogeneity $(k)$', fontsize=45)
    ax.set_ylabel(r'cost of regret $(c_2)$', fontsize=45)
    ax.grid(False)
    #ax2.grid(False)
    #ax3.grid(False)
    ratio = 1.0
    x_left, x_right = ax.get_xlim()
    y_low, y_high = ax.get_ylim()
    ax.set_aspect(abs((x_right-x_left)/(y_low-y_high))*ratio)
    clb = fig.colorbar(img, location="top", shrink=0.5)
    clb.ax.set_title(r'bayes risk ($\beta$)', fontsize=32)
    clb.ax.tick_params(labelsize=32)
    if mode=="d":
        plt.savefig("results/"+name+"/"+zName+"/"+"br_"+str(tmax)+".png")
    else:
        plt.savefig("results/"+name+"/"+zName+"/"+"br_AB.png", bbox_inches='tight', dpi=300   )
    #fig.tight_layout()
    plt.show()

#returns the minimum BR for each k,h
def getMinimumBR(name, G, tmax, zName, za, mode="AB"):
    path1 = name +"/more_a/" + zName+ "/"

    metrics = getMetrics(path1, G, tmax, mode, zName)
    accuracies = metrics[0]
    times = metrics[1]
    costs = metrics[2]
    timesList = metrics[3] 
    regrets = metrics[4]
    hList=[]
    kList=[]
    brList = []
    minima=[]

    for h in np.arange(0, 1000.1, 0.1):
        minimumBR = np.infty
        minimumBRPoint = [-1,-1]
        for k in np.arange(0, 1.05, 0.05):
            if "chci" in name:
                br = costs[int(k*20)] + h * (regrets[int(k*20)])
            elif "chds" in name:
                br = costs[int(k*20)] + h * (1-accuracies[int(k*20)])
            if br<minimumBR:
                minimumBR = br
                minimumBRPoint = [round(k,2), round(h,2)]
            hList.append(round(h,2))
            kList.append(round(k,2))
            brList.append(br)
        minima.append(minimumBRPoint)

    return minima

#plot the minimum br for G in GList
def plot_br_and_minima(name, zName, za,GList=[4,6,8,10]):
    minimumBRList = []
    for G in GList: 
        name_=name+"/G_"+str(G)   
        minimumBRList.append(getMinimumBR(name_, G, tmax, zName, za, "AB"))
    fig, ax = plt.subplots(figsize=(10, 12))
    for minima, color, name in zip(minimumBRList, ["red", "blue", "green", "purple"], ["4", "6", "8", "10"]):
        ax.scatter([x[0] for x in minima], [y[1] for y in minima], c=color, label=name)

    ax.tick_params(labelsize=30)
    ax.set_xlabel(r'swarm heterogeneity $(k)$', fontsize=45)
    ax.set_ylabel(r'cost of regret $(c_2)$', fontsize=45)

    ratio = 1.0
    x_left, x_right = ax.get_xlim()
    y_low, y_high = ax.get_ylim()
    ax.set_aspect(abs((x_right-x_left)/(y_low-y_high))*ratio)

    #plt.legend(loc="upper right")
    #plt.legend(bbox_to_anchor=(0, 1.02, 1, 0.2), loc="lower left",
    #            mode="expand", borderaxespad=0, ncol=4, title="number of counted votes with majority rule (G)", fontsize=32, title_fontsize=32)

    #fig.subplots_adjust(left=0.2, bottom=0.2, right=1, top=0.9, wspace=0, hspace=0)
    plt.show()
    plt.savefig("test.png", bbox_inches='tight')


#c_k + h (1 - alpha_k) < c_(k+1) + h(1 - alpha_(k+1))
# h( - alpha_k + alpha_(k+1) ) < c_(k+1)-c_k

#h < (c_(k+0.05)-c_k)/(alpha_k-alpha_(k+0.05)) for each k in [0,0.95, 0.05]

tmax=1000
G=8

zName = "za_0_0_5"
za=0.05
name = "chds"
path1 = name + "/G_" +str(G)+"/more_a/"+zName+"/"
mode = "AB" 
metrics = getMetrics(path1, G, tmax, mode, zName) #[accuracies, avgTimes, costs, timesList, regrets]
[accuracies, avgTimes, costs, timesList, regrets] = metrics
h_minima = []

names = ["chds", "chci"]
znames = ["za_0", "za_0_0_5"]
for name in names:
    for zName,za in zip(znames, [0,0.05]):
        print(name+" "+zName)
        plotBR(name+"/G_8", G, tmax, zName, za, mode="AB")
        #break
        #plot_br_and_minima(name, zName, za)
    #break
'''
for k in range(20):
    h_minima.append((costs[k+1]-costs[k])/(accuracies[k+1]-accuracies[k]))
print(h_minima)



for G in [4,6,8,10]:
    name = "chds/G_"+str(G)
    for za, zName in zip([0, 0.05], ["za_0", "za_0_0_5"]):
        plotBR(name, G, tmax, zName, za, mode="AB")
        break
    break
'''
