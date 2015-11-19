# install.packages( "DiagrammeR")
library(DiagrammeR)
grViz("
      digraph{
      
      # graph attributes
      graph [overlap = true]
      
      # node attributes
      node [shape = box,
      fontname = Helvetica,
      color = blue]
      
      # edge attributes
      edge [color = gray]
      
      # node statements
      
      clean_dmc [color = red]
      id_version_map [color = green]
      tar_base_all_clientes
      clean_dmc_tar_md_cont_tarjh [color = red]
      clean_dmc_tar_md_cont_operah [color = red]
      clean_dmc_com_tar_md_contratoht [color = red] 
      tar_base_full 
      tar_base_mp 
      clean_dmc_tar_md_cont_tarjh [color = red] 
      tar_tinenca
      src_sicode_gastos_tarjetas [color = green]
      src_master_tarjetas_ag [color = green]
      tar_ope_v1
      tar_mp_CREDITO_v1
      tar_mp_DEBITO_v1
      tar_mp_REVOLVING_v1
      
      tar_mp_CREDITO_v2
      tar_mp_DEBITO_v2
      tar_mp_REVOLVING_v2
      clean_dmc_tar_md_con_marg_tith [color = red]
      tar_mp_CREDITO_v3
      tar_mp_DEBITO_v3
      tar_mp_REVOLVING_v3
      tarjetas_v1

      # edge statements
      
      clean_dmc -> tar_base_all_clientes [color = black, tooltip='join']
      id_version_map  -> tar_base_all_clientes  [color = black]
      clean_dmc_tar_md_cont_tarjh ->tar_base_full  [color = grey]
      clean_dmc_com_tar_md_contratoht ->tar_base_full [color = grey]
      clean_dmc_tar_md_cont_operah ->tar_base_full [color = grey]
      tar_base_full -> tar_base_mp [color = green]

      clean_dmc_tar_md_cont_tarjh -> tar_tinenca  [color = black]
      src_master_tarjetas_ag -> tar_tinenca [color = black]

      clean_dmc_tar_md_cont_operah -> tar_ope_v1  [color = black]
      src_sicode_gastos_tarjetas -> tar_ope_v1  [color = black]

      tar_ope_v1 -> tar_ope_v2  [color = yellow]

      clean_dmc_com_tar_md_contratoht -> tar_mp_CREDITO_v1 [color = black]
      src_master_tarjetas_ag -> tar_mp_CREDITO_v1 [color = black]
      
      clean_dmc_com_tar_md_contratoht -> tar_mp_DEBITO_v1 [color = black]
      src_master_tarjetas_ag -> tar_mp_DEBITO_v1 [color = black]
      
      clean_dmc_com_tar_md_contratoht -> tar_mp_REVOLVING_v1 [color = black]
      src_master_tarjetas_ag -> tar_mp_REVOLVING_v1 [color = black]
      
      tar_mp_CREDITO_v1  -> tar_mp_CREDITO_v2 [color = yellow]
      tar_mp_DEBITO_v1 -> tar_mp_DEBITO_v2 [color = yellow]
      tar_mp_REVOLVING_v1 -> tar_mp_REVOLVING_v2 [color = yellow]
      
      tar_ope_v2 ->tar_mp_CREDITO_v3 [color = red]
      clean_dmc_tar_md_con_marg_tith ->tar_mp_CREDITO_v3 [color = orange]
      tar_mp_CREDITO_v2 ->tar_mp_CREDITO_v3 [color = orange]
      
      tar_ope_v2 ->tar_mp_DEBITO_v3 [color = red]
      clean_dmc_tar_md_con_marg_tith ->tar_mp_DEBITO_v3 [color = orange]
      tar_mp_DEBITO_v2 ->tar_mp_DEBITO_v3 [color = orange]
      
      tar_ope_v2->tar_mp_REVOLVING_v3 [color = red]
      clean_dmc_tar_md_con_marg_tith->tar_mp_REVOLVING_v3 [color = orange]
      tar_mp_REVOLVING_v2 ->tar_mp_REVOLVING_v3 [color = orange]
      
      tar_base_mp -> tarjetas_v1  [color = red]
      tar_mp_CREDITO_v3 -> tarjetas_v1 [color = orange]
      tar_mp_DEBITO_v3 -> tarjetas_v1 [color = orange]
      tar_mp_REVOLVING_v3 -> tarjetas_v1 [color = orange]

      tarjetas_v1 -> tarjetas_v2  [color = green]
      
      tarjetas_v2 -> tarjetas_v3 [color = red]
      id_version_map-> tarjetas_v3 [color = orange]
      tar_tinenca-> tarjetas_v3 [color = orange]
      
      tar_base_all_clientes -> tarjetas_v4  [color = red]
      tarjetas_v3 -> tarjetas_v4  [color = orange]
      tarjetas_v3 -> tarjetas_v4 [color = orange]
      tarjetas_v3 -> tarjetas_v4 [color = orange]
      tarjetas_v3 -> tarjetas_v4 [color = orange]

      clean_cashflow -> hist_cashflow [color = red]
      clean_cashflow -> hist_cashflow [color = orange]
      clean_cashflow -> hist_cashflow [color = orange]
      clean_cashflow -> hist_cashflow [color = orange]

      clean_dmc -> dataset_tarjetas_iteracio_2  [color = red]
      hist_cashflow -> dataset_tarjetas_iteracio_2 [color = orange]
      clean_estados -> dataset_tarjetas_iteracio_2 [color = orange]
      tarjetas_v4 -> dataset_tarjetas_iteracio_2 [color = orange]
      clean_jhp_crm_tarjetas_flags -> dataset_tarjetas_iteracio_2 [color = orange]
      
      }
      ")


