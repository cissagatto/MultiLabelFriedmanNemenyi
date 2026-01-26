##############################################################################
# Copyright (C) 2024                                                         #
#                                                                            #
# CC BY-NC-SA 4.0                                                            #
#                                                                            #
# Canonical URL https://creativecommons.org/licenses/by-nc-sa/4.0/           #
# Attribution-NonCommercial-ShareAlike 4.0 International CC BY-NC-SA 4.0     #
#                                                                            #
# Prof. Elaine Cecilia Gatto | Prof. Ricardo Cerri | Prof. Mauri Ferrandin   #
#                                                                            #
# Federal University of São Carlos - UFSCar - https://www2.ufscar.br         #
# Campus São Carlos - Computer Department - DC - https://site.dc.ufscar.br   #
# Post Graduate Program in Computer Science - PPGCC                          # 
# http://ppgcc.dc.ufscar.br - Bioinformatics and Machine Learning Group      #
# BIOMAL - http://www.biomal.ufscar.br                                       #
#                                                                            #
# You are free to:                                                           #
#     Share — copy and redistribute the material in any medium or format     #
#     Adapt — remix, transform, and build upon the material                  #
#     The licensor cannot revoke these freedoms as long as you follow the    #
#       license terms.                                                       #
#                                                                            #
# Under the following terms:                                                 #
#   Attribution — You must give appropriate credit , provide a link to the   #
#     license, and indicate if changes were made . You may do so in any      #
#     reasonable manner, but not in any way that suggests the licensor       #
#     endorses you or your use.                                              #
#   NonCommercial — You may not use the material for commercial purposes     #
#   ShareAlike — If you remix, transform, or build upon the material, you    #
#     must distribute your contributions under the same license as the       #
#     original.                                                              #
#   No additional restrictions — You may not apply legal terms or            #
#     technological measures that legally restrict others from doing         #
#     anything the license permits.                                          #
#                                                                            #
##############################################################################



##############################################################################
# WORSKSPACE
##############################################################################
FolderRoot = "~/MultiLabelFriedmanNemenyi"
FolderScripts = "~/MultiLabelFriedmanNemenyi/R"





#' Perform Friedman Test and Nemenyi Post-Hoc Analysis with Output Saving
#'
#' This function conducts a Friedman test to evaluate significant differences among multiple methods
#' based on provided data. If significant differences are detected, the function proceeds with a 
#' Nemenyi post-hoc test to identify which methods differ significantly. The results, including a 
#' difference matrix, Critical Difference (CD) plot, and p-values plot, are saved as CSV and PDF files.
#'
#' @param data A data frame where each row represents a method and each column represents an observation or measurement.
#' @param save.dir A string specifying the directory where output files will be saved.
#' @param measure.name A string used as the base name for output files.
#' @param width Numeric value indicating the width of the PDF plots.
#' @param height Numeric value indicating the height of the PDF plots.
#' @param cex Numeric value controlling the size of text in the plots.
#'
#' @return A data frame with the following columns:
#'   \describe{
#'     \item{\code{ChiSquare}}{The Chi-Square statistic from the Friedman test.}
#'     \item{\code{pValue}}{The p-value from the Friedman test.}
#'     \item{\code{Method}}{The method used for the Friedman test.}
#'     \item{\code{CriticalDifference}}{The Critical Difference from the Nemenyi test.}
#'     \item{\code{Result}}{Indicates whether the methods are significantly different.}
#'   }
#'
#' @details
#' The function saves the following files:
#' \describe{
#'   \item{CSV File}{A CSV file containing the results data frame.}
#'   \item{Difference Matrix}{A CSV file containing the difference matrix from the Nemenyi post-hoc test.}
#'   \item{CD Plot}{A PDF file containing the Critical Difference plot.}
#'   \item{p-Values Plot}{A PDF file containing the p-values plot.}
#' }
#'
#' @examples
#' # Example usage:
#' # Assuming 'my_data' is a data frame and 'output_dir' is the directory for saving outputs
#' result <- friedman.nemenyi(my_data, "output.dir", "my.measure", 7, 5, 1.2)
#'
#' @export
friedman.nemenyi <- function(data, save, measure.name, 
                             width = 7, height = 5, cex = 1.2) {
  
  # Perform the Friedman test
  fr <- friedmanTest(data)
  ne <- nemenyiTest(data, alpha = 0.05)
  
  # Save the Critical Difference (CD) plot as a PDF
  cd.plot.file <- file.path(save, paste0(measure.name, "-CD-plot.pdf"))
  pdf(cd.plot.file, width = width, height = height)
  plotCD(data, alpha = 0.05, cex = cex)
  dev.off()
  gc()
  
  # Extract relevant statistics
  ChiSquare <- fr$statistic
  pValue <- fr$p.value
  Method <- fr$method
  CriticalDifference <- ne$statistic
  
  # Determine significance
  Result <- ifelse(pValue < 0.05, "Methods are significantly different", 
                   "Methods are not significantly different")
  
  # Compile results into a data frame
  results.df <- data.frame(ChiSquare, pValue, Method, CriticalDifference, 
                           Result, stringsAsFactors = FALSE)
  
  # Check if the diff.matrix has data
  if (!is.null(ne$diff.matrix) && nrow(ne$diff.matrix) > 0) {
    p.values.plot.file <- file.path(save, paste0(measure.name, "-pValues-plot.pdf"))
    pdf(p.values.plot.file, width = 10, height = 6)
    print(plotPvalues(ne$diff.matrix, show.pvalue = TRUE, font.size = 2))
    dev.off()
    gc()
  } else {
    warning("The difference matrix is empty; no p-values plot will be generated.")
  }
  
  
  # Save results data frame to a CSV file
  results.file <- file.path(save, 
                            paste0(measure.name, "-results.csv"))
  write.csv(results.df, results.file, row.names = FALSE)
  
  # Save the difference matrix to a CSV file
  diff.matrix.file <- file.path(save, 
                                paste0(measure.name, "-diff-matrix.csv"))
  write.csv(ne$diff.matrix, diff.matrix.file, row.names = FALSE)
  
  # # Check if the data is valid for density plotting
  # if (!is.null(data) && nrow(data) > 0 && !all(is.na(data))) {
  #   
  #   # Remove NAs from each column
  #   data.cleaned <- as.data.frame(lapply(data, function(x) x[!is.na(x)]))
  #   
  #   # Check if there is still data left to plot
  #   if (all(sapply(data.cleaned, function(x) length(x) == 0))) {
  #     warning("After removing missing values, there is not enough data left to generate the density plot.")
  #   } else {
  #     p.density.plot.file <- file.path(save, paste0(measure.name, "-density.pdf"))
  #     pdf(p.density.plot.file, width = 10, height = 6)
  #     print(plotDensities(data = data.cleaned))
  #     dev.off()
  #     gc()
  #   }
  #   
  # } else {
  #   warning("Data is empty or contains only missing values; the density plot will not be generated.")
  # }
  # 
  # Inform the user where the files have been saved
  message("Results have been saved to: ", save)
  cat("\n")
  
  # Return the results data frame
  return(results.df)
}


#' Generate Boxplots of Method Performance
#'
#' This function generates boxplots for the performance of various methods
#' based on the provided data. It saves the plots as PDF files in the specified
#' directory.
#'
#' @param data A data frame containing performance metrics for different methods.
#' @param methods A character vector specifying the names of the methods (columns)
#'                in the data frame.
#' @param save.dir A character string specifying the directory where the boxplots 
#'                 will be saved.
#'
#' @return A boxplot of the performance of the specified methods.
#' @export
#'
#' @examples
#' generate.boxplots.1(data = results_data, methods = c("Method1", "Method2"), save.dir = "output_directory")
generate.boxplots.1 <- function(data, 
                              methods, 
                              save.dir,
                              measure.name,
                              width,
                              height) {
  # Convert the data from wide format to long format for ggplot
  long.data <- data %>%
    pivot_longer(cols = all_of(methods), names_to = "method",
                 values_to = "performance")
  
  # Create the boxplot with methods ordered according to the original data frame
  long.data$method <- factor(long.data$method, levels = methods)
  
  # Generate a color palette with 24 distinct colors
  palette.colors <- colorRampPalette(RColorBrewer::brewer.pal(9, "Set1"))(length(methods))
  
  # Create the boxplot
  boxplot <- ggplot(long.data, aes(x = method, y = performance, fill = method)) +
    geom_boxplot() +
    theme_minimal() +
    labs(title = "Boxplots Performance",
         x = "Methods",
         y = "Performance") +
    theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
    scale_fill_manual(values = palette.colors)
  
  # Save the boxplot as a PDF file
  name = paste(measure.name, "-boxplots-performance.pdf", sep="")
  ggsave(file.path(save.dir, name), plot = boxplot, width = 10, height = 6)
  
  # Display the boxplot
  #print(boxplot)
}




#' Generate Boxplots of Method Performance with Mean Labels
#'
#' This function generates boxplots for the performance of various methods
#' and replaces method names on the x-axis with their mean performance.
#' It also removes the legend from the plot.
#'
#' @param data A data frame containing performance metrics for different methods.
#' @param methods A character vector specifying the names of the methods (columns)
#'                in the data frame.
#' @param save.dir A character string specifying the directory where the boxplots 
#'                 will be saved.
#'
#' @return A boxplot of the performance of the specified methods with mean labels.
#' @export
#'
#' @examples
#' generate.boxplots.2(data = results_data, methods = c("Method1", "Method2"), save.dir = "output_directory")
generate.boxplots.2 <- function(data, methods, save.dir) {
  # Convert the data from wide format to long format for ggplot
  long.data <- data %>%
    pivot_longer(cols = all_of(methods), names_to = "method", values_to = "performance")
  
  # Calculate mean performance for each method
  method.means <- long.data %>%
    group_by(method) %>%
    summarise(mean.performance = mean(performance, na.rm = TRUE)) %>%
    arrange(match(method, methods))
  
  # Create new labels with mean performance
  mean.labels <- paste0(method.means$method, "\nMean: ", round(method.means$mean.performance, 2))
  
  # Generate a color palette with 24 distinct colors (if needed)
  palette.colors <- colorRampPalette(RColorBrewer::brewer.pal(9, "Set1"))(length(methods))
  
  # Create the boxplot without the legend and with mean labels on x-axis
  boxplot <- ggplot(long.data, aes(x = method, y = performance, fill = method)) +
    geom_boxplot() +
    theme_minimal() +
    labs(title = "Boxplots of Method Performance",
         x = "Methods",
         y = "Performance") +
    theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
    scale_fill_manual(values = palette.colors) +
    scale_x_discrete(labels = mean.labels) +  # Replace method names with mean performance
    guides(fill = "none")  # Remove the legend
  
  # Save the boxplot as a PDF file
  ggsave(file.path(save.dir, "boxplots_performance.pdf"), plot = boxplot, width = 10, height = 6)
  
  # Display the boxplot
  print(boxplot)
}


#' Perform Friedman Test and Nemenyi Post-Hoc Analysis with Output Saving
#'
#' This function performs a Friedman test to evaluate statistically significant
#' differences among multiple methods. If significant differences are detected,
#' a Nemenyi post-hoc test is applied. The function automatically generates and
#' saves summary statistics, a Critical Difference (CD) plot, a p-values plot,
#' and CSV files with the results.
#'
#' The output plots can be generated either as vector-based PDF files or as
#' rasterized PNG images. Using PNG is recommended when dealing with a large
#' number of methods, as it significantly reduces file size and improves
#' compilation performance in LaTeX/Overleaf environments.
#'
#' @param data A data frame where each row represents a method and each column
#'   represents an observation or measurement.
#'
#' @param save A character string specifying the directory where output files
#'   will be saved.
#'
#' @param measure.name A character string used as the base name for all output
#'   files.
#'
#' @param width Numeric value specifying the width of the output plots (in inches).
#'
#' @param height Numeric value specifying the height of the output plots (in inches).
#'
#' @param cex Numeric value controlling the size of text elements in the plots.
#'
#' @param device Character string indicating the graphics device to be used for
#'   the plots. Possible values are \code{"pdf"} (vector graphics, default) and
#'   \code{"png"} (raster graphics).
#'
#' @param dpi Numeric value specifying the resolution (dots per inch) for PNG
#'   output. This parameter is only used when \code{device = "png"}.
#'
#' @details
#' The function generates the following output files:
#' \itemize{
#'   \item A CSV file containing the Friedman test statistics.
#'   \item A CSV file containing the Nemenyi post-hoc difference matrix.
#'   \item A Critical Difference (CD) plot comparing the average ranks of the methods.
#'   \item A p-values plot visualizing pairwise post-hoc comparisons.
#' }
#'
#' When \code{device = "pdf"}, plots are saved as vector graphics using
#' \code{useDingbats = FALSE} to ensure compatibility with LaTeX compilers.
#' When \code{device = "png"}, plots are rasterized at the specified resolution,
#' producing significantly smaller files for complex CD plots.
#'
#' @return
#' A data frame with the following columns:
#' \describe{
#'   \item{ChiSquare}{Chi-square statistic from the Friedman test.}
#'   \item{pValue}{P-value associated with the Friedman test.}
#'   \item{Method}{Name of the statistical test used.}
#'   \item{CriticalDifference}{Critical Difference value from the Nemenyi test.}
#'   \item{Result}{Textual interpretation of statistical significance.}
#' }
#'
#' @seealso
#' \code{\link{friedmanTest}}, \code{\link{nemenyiTest}}, \code{\link{plotCD}},
#' \code{\link{plotPvalues}}
#'
#' @examples
#' \dontrun{
#' # Example usage with PNG output (recommended for Overleaf)
#' result <- friedman.nemenyi.new(
#'   data = my_data,
#'   save = "output_dir",
#'   measure.name = "my_measure",
#'   width = 14,
#'   height = 7,
#'   cex = 2,
#'   device = "png",
#'   dpi = 150
#' )
#'
#' # Example usage with PDF output
#' result <- friedman.nemenyi.new(
#'   data = my_data,
#'   save = "output_dir",
#'   measure.name = "my_measure"
#' )
#' }
#'
#' @export
friedman.nemenyi.new <- function(
    data,
    save,
    measure.name,
    width = 7,
    height = 5,
    cex = 1.2,
    device = c("pdf", "png"),
    dpi = 150
) {
  
  device <- match.arg(device)
  
  # Helper to open the correct graphics device
  open_device <- function(filename, width, height) {
    if (device == "pdf") {
      pdf(
        filename,
        width = width,
        height = height,
        useDingbats = FALSE
      )
    } else {
      png(
        filename,
        width = width * dpi,
        height = height * dpi,
        res = dpi
      )
    }
  }
  
  # Perform the Friedman test
  fr <- friedmanTest(data)
  ne <- nemenyiTest(data, alpha = 0.05)
  
  ext <- ifelse(device == "pdf", "pdf", "png")
  
  # ---- CD Plot ----
  cd.plot.file <- file.path(
    save,
    paste0(measure.name, "-CD-plot.", ext)
  )
  
  open_device(cd.plot.file, width, height)
  plotCD(data, alpha = 0.05, cex = cex)
  dev.off()
  gc()
  
  # Extract relevant statistics
  ChiSquare <- fr$statistic
  pValue <- fr$p.value
  Method <- fr$method
  CriticalDifference <- ne$statistic
  
  # Determine significance
  Result <- ifelse(
    pValue < 0.05,
    "Methods are significantly different",
    "Methods are not significantly different"
  )
  
  # Compile results into a data frame
  results.df <- data.frame(
    ChiSquare,
    pValue,
    Method,
    CriticalDifference,
    Result,
    stringsAsFactors = FALSE
  )
  
  # ---- p-values Plot ----
  if (!is.null(ne$diff.matrix) && nrow(ne$diff.matrix) > 0) {
    
    p.values.plot.file <- file.path(
      save,
      paste0(measure.name, "-pValues-plot.", ext)
    )
    
    open_device(p.values.plot.file, 10, 6)
    print(
      plotPvalues(
        ne$diff.matrix,
        show.pvalue = TRUE,
        font.size = 2
      )
    )
    dev.off()
    gc()
    
  } else {
    warning(
      "The difference matrix is empty; no p-values plot will be generated."
    )
  }
  
  # ---- Save CSV outputs ----
  results.file <- file.path(
    save,
    paste0(measure.name, "-results.csv")
  )
  write.csv(results.df, results.file, row.names = FALSE)
  
  diff.matrix.file <- file.path(
    save,
    paste0(measure.name, "-diff-matrix.csv")
  )
  write.csv(ne$diff.matrix, diff.matrix.file, row.names = FALSE)
  
  message("Results have been saved to: ", save)
  cat("\n")
  
  return(results.df)
}




##############################################################################
# 
##############################################################################
