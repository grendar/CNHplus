## ----setup, include = FALSE---------------------------------------------------
knitr::opts_chunk$set(
  collapse = TRUE,
  comment = "#>"
)

## ----message = FALSE----------------------------------------------------------
library('CNHplus')
library('TCGAbiolinks')
library('stringi')
library('openxlsx')
library('dplyr')

## -----------------------------------------------------------------------------
data(sample_data)
head(sample_data)

## -----------------------------------------------------------------------------
sa = sample_data
sample_name = sa$Sample[1]
r = 2^sa$Segment_Mean
w = sa$End - sa$Start # should be +1

## ---- fig.width=7, fig.height=4, include=T, fig.align='center', fig.show='hold', results=FALSE----
# RCN profile plot
plot_profile(sa, r, paste0(sample_name, '_RCN.tiff'),
             ylim = c(0,5), ylab = 'RCN')

## -----------------------------------------------------------------------------
# making grid
grid = make_grid(purity = seq(0.2, 1, 0.01), ploidy = seq(1.5, 5, 0.01))

## -----------------------------------------------------------------------------
# ACN profile for CNH
cnh = find_cnhplus(grid, r = r, w = w, k=2, plus = F)
cnh

## -----------------------------------------------------------------------------
acn_cnh = r2q(r, cnh$purity[1], cnh$ploidy[1])

## ---- fig.width=7, fig.height=4, include=T, fig.align='center', fig.show='hold', results=FALSE----
plot_profile(sa, acn_cnh, paste0(sample_name, '_ACN_for_CNH.tiff'),
             ylim = c(-10,10), ylab = 'ACN for CNH')

## ---- fig.width=7, fig.height=4, include=T, fig.align='center', fig.show='hold', results=FALSE----
# ACN profile for CNH+
cnh_plus = find_cnhplus(grid, r = r, w = w, k=2, plus = T)
cnh_plus
#
acn_cnhplus = r2q(r, cnh_plus$purity[1], cnh_plus$ploidy[1])
plot_profile(sa, acn_cnhplus, paste0(sample_name, '_ACN_for_CNHplus.tiff'),
             ylim = c(0,10), ylab = 'ACN for CNH+')

## -----------------------------------------------------------------------------
# TCGA study
study_name = 'READ'
#
query = TCGAbiolinks::GDCquery(legacy = T,
                 project = paste0('TCGA-', study_name), 
                 data.category = "Copy number variation",
                 file.type = 'hg19.seg',
                 platform = "Affymetrix SNP Array 6.0",
                 sample.type = 'Primary Tumor')
# download segmented_scna_hg19 data 
TCGAbiolinks::GDCdownload(query = query, 
            method = 'api', files.per.chunk = 10) #'client') # 
data = TCGAbiolinks::GDCprepare(query = query)
da = as.data.frame(data)
#            

## -----------------------------------------------------------------------------
# read in the Supplement to van Dijk et al. with results for tumor samples from TCGA studies
vD = openxlsx::read.xlsx("https://static-content.springer.com/esm/art%3A10.1038%2Fs41467-021-23384-6/MediaObjects/41467_2021_23384_MOESM4_ESM.xlsx")
#

## -----------------------------------------------------------------------------
# filter samples from the study
vDx = vD %>% filter(Type == study_name)
#
# match vD and TCGA
im = match(vDx$Samplename, unique(da$Sample))
#
# data frame with RCN for the study samples which were considered by van Dijk et al.
da_vD = da %>% filter(Sample %in% unique(da$Sample)[im])
#

## ---- cache = TRUE------------------------------------------------------------
# grid has already been prepared, above
#
# analyze TCGA study
oo = analyze_TCGA_study(study_name, da_vD, grid, k=2)
#

## ---- fig.width=7, fig.height=8.5, include=T, fig.align='center', fig.show='hold'----
# read in the results file for the study
res = read.csv(paste0(study_name, '_results.csv'))
# match samples from results file and the survival data in vDx
im_vDx_res = match(vDx$Samplename, res$sample)
# KM for two groups; below/above median CNH+
gg_cnhplus = plot_survival(study_name, vDx$OS, vDx$OS_event, 
                           res$cnh_plus[im_vDx_res],
                           type = 'CNH+', ylim = c(0, 1))
gg_cnhplus

