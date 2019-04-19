data_raw <- base::system.file("extdata/animalcules.rds", package = "animalcules") %>%
            base::readRDS()

se_mgx <- magrittr::use_series(data_raw, count_table) %>%
          base::data.matrix() %>%
          S4Vectors::SimpleList() %>%
          magrittr::set_names("MGX")

se_ge <- magrittr::use_series(data_raw, gene_expression_table) %>%
         base::data.matrix() %>%
         S4Vectors::SimpleList() %>%
         magrittr::set_names("GeneExpression")

se_colData <- magrittr::use_series(data_raw, metadata_table) %>%
              S4Vectors::DataFrame()

se_rowData <- magrittr::use_series(data_raw, tax_table) %>%
              base::data.frame() %>%
              dplyr::mutate_all(as.character) %>%
              dplyr::select(superkingdom, phylum, class, order, family, genus) %>%
              S4Vectors::DataFrame()

microbe_se <- SummarizedExperiment::SummarizedExperiment(assays = se_mgx,
                                                         colData = se_colData,
                                                         rowData = se_rowData)

host_se <- SummarizedExperiment::SummarizedExperiment(assays = se_ge,
                                                      colData = se_colData)

mae_experiments <- S4Vectors::SimpleList(MicrobeGenetics = microbe_se, 
                                         HostGenetics = host_se)

MAE <- MultiAssayExperiment::MultiAssayExperiment(experiments = mae_experiments, 
                                                  colData = se_colData)

saveRDS(MAE, "extdata/MAE.rds")

microbe <- MAE[['MicrobeGenetics']] #double bracket subsetting is easier
tax_table <- as.data.frame(rowData(microbe)) # organism x taxlev
sam_table <- as.data.frame(colData(microbe)) # sample x condition
counts_table <- as.data.frame(assays(microbe))[,rownames(sam_table)] # organism x sample

toy_data <- list("tax_table"=tax_table,
                 "sam_table"=sam_table,
                 "counts_table"=counts_table)     

saveRDS(toy_data, "extdata/toy_data.rds")