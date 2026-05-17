#===============================================================================
#----------Data-&-function-defining----------
#===============================================================================
data <- read.csv("Telugu_edited.csv")
colnames(data) <- 1:68


kendall_sig <- function(col){
  
  pvals <- c()
  
  for(i in 15:68){
    
    test <- cor.test(data[,col], data[,i], method="kendall")
    
    pvals <- c(pvals, test$p.value)
    
  }
  adjusted_pvals <- p.adjust(pvals, method = "BH")
  return(which(adjusted_pvals < 0.05) + 14)
}
#===============================================================================
#----Data_Omission----
#===============================================================================
data <- data[ !(data[,22] %in% c(-1,-2) & data[,33] %in% c(-1,-2)), ]
data <- data[ !(data[,22] %in% c(1,2) & data[,33] %in% c(1,2)), ]

data <- data[ !(data[,41] %in% c(1,2) & data[,53] %in% c(-1,-2)), ]
data <- data[ !(data[,41] %in% c(-1,-2) & data[,53] %in% c(1,2)), ]

data <- data[ !(data[,43] %in% c(1,2) & data[,53] %in% c(-1,-2)), ]
data <- data[ !(data[,41] %in% c(-1,-2) & data[,53] %in% c(1,2)), ]

#===============================================================================
#====Descriptive-Statistic====
#===============================================================================

#Given the economic status of Indians, possessing nuclear weapons is a burden on Indians.
mean(data[,17] %in% c(-1,-2)) * 100

#India's nuclear weapons program has prevented others from attacking India.
mean(data[,19] %in% c(1,2)) * 100

#There should be international controls on more countries having nuclear weapons.
mean(data[,21] %in% c(1,2)) * 100

#Nuclear-armed countries should lower the number of nuclear weapons they have.
mean(data[,34] %in% c(-1,-2)) * 100

#There should be more efforts to educate the public about the effects of nuclear weapons.
mean(data[,36] %in% c(1,2)) * 100

#If all other countries, including U.S.A. and Russia,  give up their nuclear weapons, India should also give up its nuclear weapons.
mean(data[,39] %in% c(-1,-2)) * 100

#If Pakistan and China give up their nuclear weapons India should also give up its nuclear weapons.
mean(data[,45] %in% c(1,2)) * 100

#Nuclear weapons cannot destroy the whole world
mean(data[,16] %in% c(-1,-2)) * 100

#I will immigrate to a foreign country if I had better career opportunity.
mean(data[,49] %in% c(0)) * 100

#India should start compulsory military service at the age of 18 for 1 year.
mean(data[,51] %in% c(1,2)) * 100

#India should focus more on developing conventional military capabilities rather than investing in nuclear 
mean(data[,54] %in% c(1,2)) * 100

#Pakistan having nuclear weapons has made Pakistanis safer.
mean(data[,59] %in% c(1,2)) * 100

#Nuclear weapons play a role in maintaining global peace.
mean(data[,64] %in% c(1,2)) * 100

#Would your answers be the same, if you belonged to a non-nuclear country?
mean(data[,68] %in% c(1)) * 100


x <- data[,68]

labels_map <- c(
  
  "0"  = "No",
  "1"  = "Yes"
)

freq <- table(x)

pct <- round(freq/sum(freq)*100,1)

colors <- c("#fc8d59","#91bfdb" )

pie(freq,
    labels = paste(pct, "%"),
    col = colors,
    main = "Responses")

legend("topleft",
       legend = labels_map[names(freq)],
       fill = colors,
       cex = 0.6)



#===============================================================================
#=======Kendall's tau test for association======
#===============================================================================
#How are they associated
Tau_table <-  cor(data[,15:68],
                  method = "kendall",
                  use = "pairwise.complete.obs")

#Given the economic status of Indians, possessing nuclear weapons is a burden on Indians.
kendall_sig(17)
for(k in kendall_sig(17)){
  
  print(Tau_table[17-14, k-14])
  
}
#Indian States should allow mass immigration

kendall_sig(18)
for(k in kendall_sig(18)){
  
  print(Tau_table[18-14, k-14])
  
}
#iNDIA'S NUCLEAR WEAPON'S PROGRAM HAS PREVENNTED INDIA FROM ATTACKS
c<-19
kendall_sig(c)
for(k in kendall_sig(c)){
  
  print(Tau_table[c-14, k-14])
  
}

#India should get rid of nuclear weapons

c<-22
kendall_sig(c)
for(k in kendall_sig(c)){
  
  print(Tau_table[c-14, k-14])
  
}

#if pakistan give up there nuc weap India should too

c<-24
kendall_sig(c)
for(k in kendall_sig(c)){
  
  print(Tau_table[c-14, k-14])
  
}

#India's nuc wep prog has helped india in its regional stability
c<-25
kendall_sig(c)
for(k in kendall_sig(c)){
  
  print(Tau_table[c-14, k-14])
  
}

#A nuclear weapon will cause killing of 1,00,000 people or more
c<-29
kendall_sig(c)
for(k in kendall_sig(c)){
  
  print(Tau_table[c-14, k-14])
  
}

#India should put more efforts in educating people about nuclear weapons
c<-36
kendall_sig(c)
for(k in kendall_sig(c)){
  
  print(Tau_table[c-14, k-14])
  
}

#If usa,russa give up nuc wep then india also should
c<-39
kendall_sig(c)
for(k in kendall_sig(c)){
  
  print(Tau_table[c-14, k-14])
  
}

#India should welcome permanent migrants from diff cont
c<-40
kendall_sig(c)
for(k in kendall_sig(c)){
  
  print(Tau_table[c-14, k-14])
  
}

#If pak and china give up nuc wep then india also should
c<-45
kendall_sig(c)
for(k in kendall_sig(c)){
  
  print(Tau_table[c-14, k-14])
  
}

#Final decision to use nuclear wep should be left to PM alone
c<-43
kendall_sig(c)
for(k in kendall_sig(c)){
  
  print(Tau_table[c-14, k-14])
  
}

#Both PM and Leader of Opp should be responsible for taking final decisions for using nuc wep

c<-31
kendall_sig(c)
for(k in kendall_sig(c)){
  
  print(Tau_table[c-14, k-14])
  
}

#I will imm to foriegn cont if I had better opp
c<-49
kendall_sig(c)
for(k in kendall_sig(c)){
  
  print(Tau_table[c-14, k-14])
  
}

#there should be global reg abour usage of nuc weap
c<-50
kendall_sig(c)
for(k in kendall_sig(c)){
  
  print(Tau_table[c-14, k-14])
  
}

#India should start com military ser
c<-51
kendall_sig(c)
for(k in kendall_sig(c)){
  
  print(Tau_table[c-14, k-14])
  
}

#India should focus more on conv wep rather than nuc wep
c<-54
kendall_sig(c)
for(k in kendall_sig(c)){
  
  print(Tau_table[c-14, k-14])
  
}

#the risk of nuclear war has inc in last 5 years
c<-56
kendall_sig(c)
for(k in kendall_sig(c)){
  
  print(Tau_table[c-14, k-14])
  
}

# india should be more transparent about its nuc prob to public
c<-58
kendall_sig(c)
for(k in kendall_sig(c)){
  
  print(Tau_table[c-14, k-14])
  
}

#pak having nuc ha smade pak safer
c<-59
kendall_sig(c)
for(k in kendall_sig(c)){
  
  print(Tau_table[c-14, k-14])
  
}

#india should focs on nuc tech than nuc wep
c<-60
kendall_sig(c)
for(k in kendall_sig(c)){
  
  print(Tau_table[c-14, k-14])
  
}
#nuc wep play role in mian taining glob peace
c<-64
kendall_sig(c)
for(k in kendall_sig(c)){
  
  print(Tau_table[c-14, k-14])
  
}

#questions effected by satisfaction of family financial status
c<-67
kendall_sig(c)
for(k in kendall_sig(c)){
  
  print(Tau_table[c-14, k-14])
  
}

#would ur answers be same if u were from a non-nuc cont
c<-68
kendall_sig(c)
for(k in kendall_sig(c)){
  
  print(Tau_table[c-14, k-14])
  
}
#===============================================================================
#what are effected by gender
#===============================================================================
I<-c()
for (i in 15:68) {
wilcox <- wilcox.test(data[,i] ~ data[,3], data = data)
  if(p.adjust(wilcox$p.value, method = "BH") < 0.05)
  I<-c(I,i)
}
I

#===============================================================================
#-----------------------------Network-Plot--------------------------------------
#===============================================================================
library(igraph)

target_cols <- 15:68
n_items <- length(target_cols)

# 2. Re-initialize the raw p-value matrix
p_mat <- matrix(NA, nrow = n_items, ncol = n_items)
rownames(p_mat) <- colnames(p_mat) <- target_cols

# 3. Harvest raw p-values from your dataset
for (i in 1:(n_items - 1)) {
  for (j in (i + 1):n_items) {
    col_i <- target_cols[i]
    col_j <- target_cols[j]
    
    # Suppress warnings about exact p-values with ties
    test <- suppressWarnings(cor.test(data[, col_i], data[, col_j], method = "kendall"))
    p_mat[i, j] <- test$p.value
    p_mat[j, i] <- test$p.value # Mirror the matrix
  }
}

# 4. Extract unique pairs and apply the global Benjamini-Hochberg adjustment
raw_pvals <- p_mat[upper.tri(p_mat)]
adj_pvals <- p.adjust(raw_pvals, method = "BH")

# 5. Build the final adjusted p-value matrix (adj_p_mat)
adj_p_mat <- matrix(0, nrow = n_items, ncol = n_items)
adj_p_mat[upper.tri(adj_p_mat)] <- adj_pvals
adj_p_mat <- adj_p_mat + t(adj_p_mat) # Mirror it
rownames(adj_p_mat) <- colnames(adj_p_mat) <- target_cols
#===============================================================================
# Corrected Filtering: Raw Significance AND Practical Strength
#===============================================================================

# Start with your raw Kendall correlation matrix
tau_mat <- cor(data[,15:68],
               method = "kendall",
               use = "pairwise.complete.obs")
adj_mat <- tau_mat

# Filter 1: Use RAW p-values (p_mat) at the 5% level test
# This ensures each individual line is statistically significant 
adj_mat[p_mat >= 0.05] <- 0

# Filter 2: Apply a reasonable effect size threshold
# (0.25 is excellent for Kendall's Tau since Tau values naturally run smaller than Pearson)
adj_mat[abs(adj_mat) < 0.3] <- 0

# Remove self-loops
diag(adj_mat) <- 0

#===============================================================================
# 2. Build and Plot the Perfect Network
#===============================================================================

# Build the graph object from your dual-filtered matrix
g <- graph_from_adjacency_matrix(adj_mat,
                                 mode = "undirected",
                                 weighted = TRUE)

# Edge colors: blue = positive, red = negative
E(g)$color <- ifelse(E(g)$weight > 0, "blue", "red")

# Create absolute weights for layout and clustering algorithms
E(g)$abs_weight <- abs(E(g)$weight)

#===============================================================================
# CRITICAL REPRODUCIBILITY FIX: Freeze the Random Number Generator
#===============================================================================
#494 
set.seed(69)


# Run community detection on the cleaned matrix
cl <- cluster_louvain(g, weights = E(g)$abs_weight)

# Generate a fixed layout coordination matrix
fixed_layout <- layout_with_fr(g, weights = E(g)$abs_weight)

# Plot the beautiful, publication-ready network
community_colors <- c("lightsalmon2", "#FFBF00", "olivedrab3","palegreen3","lightseagreen","#0040FF","#8000FF", "#FF00BF")

plot(cl, g,
             layout = fixed_layout,
             col = community_colors[membership(cl)],
             vertex.size = 7,
             vertex.label.cex = 0.8,
             edge.width = E(g)$abs_weight * 5,
             main = "Questionnaire Network (p < 0.05 & |tau|>=0.3.)")

legend(x = -1.8, y = 1,
       legend = paste0("C", 1:length(community_colors)),
       col = community_colors,
       pch = 19,
       pt.cex = 2,
       cex = 0.8,
       bty = "n")
membership_vec <- igraph::membership(cl)
split(names(membership_vec), membership_vec)

#===============================================================================
# Community Association Matrix
# Mean absolute Kendall Tau between communities
#===============================================================================

# Community memberships
membership_vec <- membership(cl)

# Number of communities
k <- length(unique(membership_vec))

# Empty matrix
community_assoc <- matrix(NA, nrow = k, ncol = k)

rownames(community_assoc) <- paste0("C", 1:k)
colnames(community_assoc) <- paste0("C", 1:k)

# Compute mean absolute association between communities
for(a in 1:k){
  
  for(b in 1:k){
    
    nodes_a <- which(membership_vec == a)
    nodes_b <- which(membership_vec == b)
    
    vals <- c()
    
    for(i in nodes_a){
      
      for(j in nodes_b){
        
        # avoid duplicate/self-pairs inside same community
        if(a == b && i >= j) next
        
        vals <- c(vals, as.numeric(abs(tau_mat[i,j])))
      }
    }
    
    community_assoc[a,b] <- mean(vals, na.rm = TRUE)
  }
}

# Show matrix
community_assoc

# Optional heatmap
heatmap(community_assoc,
        Rowv = NA,
        Colv = NA,
        scale = "none",
        margins = c(7,7),
        main = "Association Between Communities")
