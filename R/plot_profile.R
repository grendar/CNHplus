#'
#' Plots CN profile
#'
#' \code{plot_profile} makes gtrellis plot of CN profile over all chromosomes.
#' Sample, Chromosome, Start, End and value of CN at the segment are the input.
#'
#' @param sa a data frame which contains Sample Chromosome Start End
#' @param cn vector giving CN profile for the segments specified in sa
#' @param filename name of file where the plot will be stored
#' @param ylim vector of two elements giving the lower, upper values to which limit CN
#' @param ylab label to be placed on y axis (default 'CN')
#' @return invisibly, saves the plot as a tiff
#' @examples
#' \dontrun{
#' # Ex: profile plot for a sample;
#' #     assumes that example from analyze_TCGA_study is already run
#' #
#' sample_names = unique(da_vD$Sample)
#' i=1
#' sample_names[i]
#' #[1] "TCGA-AF-2687-01A-02D-1732-01"
#' sa = da %>% filter(Sample == sample_names[i])
#' #
#' # RCN
#' r = 2^sa$Segment_Mean
#' w = w = sa$End - sa$Start # should be +1
#' plot_profile(sa, r, paste0(sample_names[i], '_RCN.tiff'),
#'              ylim = c(0,3), ylab = 'RCN')
#' #
#' # ACN for CNH (unfiltered)
#' cnh = find_cnhplus(grid, r = r, w = w, k=2, plus = F)
#' cnh
#' #   purity ploidy     kappa
#' #1   0.25   2.99 0.1733696
#' #2   0.25   3.00 0.1734264
#'
#' acn_cnh = r2q(r, cnh$purity[1], cnh$ploidy[1])
#' plot_profile(sa, acn_cnh, paste0(sample_names[i], '_ACN_for_CNH.tiff'),
#'             ylim = c(-10,10), ylab = 'ACN for CNH')
#' #
#' # ACN for CNH+
#' # oo[[i]]
#' #                   sample_name purity ploidy     kappa
#' #1 TCGA-AF-2687-01A-02D-1732-01      1   1.94 0.1880932
#' #2 TCGA-AF-2687-01A-02D-1732-01      1   1.93 0.1883289
#'
#' acn_cnhplus = r2q(r, oo[[i]]$purity[1], oo[[i]]$ploidy[1])
#' plot_profile(sa, acn_cnhplus, paste0(sample_names[i], '_ACN_for_CNHplus.tiff'),
#'              ylim = c(-10,10), ylab = 'ACN for CNH+')
#' }
#' @seealso \code{analyze_TCGA_study}
#' @export
plot_profile = function(sa, cn, filename, ylim = NULL, ylab = 'CN') {
  #
  checkmate::assert(
    checkmate::check_data_frame(sa),
    checkmate::check_vector(cn),
    checkmate::check_character(filename)
  )
  #
  #
  sample_name = sa$Sample[1]
  #
  if (is.null(ylim) == TRUE) {
    cn_range = c(min(cn)-0.1, max(cn)+0.1)
  } else {
    cn_range = ylim
  }
  # chrs
  chrs = names(table(sa$Chromosome)) # vector of strings
  i23 = which(chrs == '23')
  i24 = which(chrs == '24')
  if (length(i23) > 0) {
    sa$Chromosome[sa$Chromosome == 23] = 'X'
  }
  if (length(i24) > 0) {
    sa$Chromosome[sa$Chromosome == 24] = 'Y'
  }
  #
  gtrellis::gtrellis_layout(track_ylim = cn_range,
                            xlab = '',
                            gap = 0,
                            title = sample_name,
                            title_fontsize = 12,
                            axis_label_fontsize = 12,
                            xaxis = FALSE,
                            remove_chr_prefix = TRUE,
                            add_name_track = TRUE,
                            track_ylab = ylab)
  #
  gtrellis::add_segments_track(sa[,2:4],
                               value = cn,
                               gp = grid::gpar(lwd = 4, col = 'blue'))
  #
  gtrellis::add_points_track(sa[,2:4], track = gtrellis::current_track(),
                             value = cn,
                             gp = grid::gpar(cex=2, col = 'blue'))

  # save it
  dev.copy(tiff,
           filename,
           height = 100,
           width = 300,
           units = 'mm',
           res = 300,
           pointsize = 6,
           compression = 'lzw')
  dev.off()
  #
}
