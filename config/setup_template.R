require(ckanr)
CKAN="http://internal-data.dpaw.wa.gov.au/"
APIKEY="my-api-key"
ckanr::ckanr_setup(url=CKAN, key=APIKEY)

# CKAN resource IDs
MDB_RID="9e81c360-cfe0-4fc0-87f7-d3b64ec57be4"
ETL_RID="1df672e2-4e42-4145-8be2-40c02a8f9319"
SITES_RID="efdced11-ced6-42aa-92a9-259bc720ae28"
SITES_CSV_RID="dd3a8cff-2488-4ea0-990f-103984197a4c"
SURVEYS_RID="dbd6d236-a478-42f3-8731-5bc7dc12666e"
CRAWL_RID="b8781eff-d807-4096-8ffe-0a0a8af7abcf"
NEST_RID="477ada0f-9116-4f9a-b282-2e5c4fe803d6"
NEW_NEST_RID="24e90f53-e696-44cb-b547-4e800d819137"
FALSE_CRAWL_RID="1c7ec216-bc28-4181-bf8a-2d1528f61717"
SPM_RID="422c91ca-7673-432f-911a-449d3dc2e35a"
NEST_SEASON_RID="bfd2e01c-6b8b-4b62-942c-01e5ecb60a39"

W2_ETL_RID="14640752-429f-46ad-9604-f76c9d9a3599"
W2_ENC_CSV_RID="f7e30448-de1c-4061-8a85-887632934961"

TRACKS_RID="e5169971-dcc4-4e6a-b69c-cfcbfd0a285e"

# MS SQL Server for tagging db
SRV='db-server'
DB='db-name'
UN='db-username'
PW='db-password'
TAG_CON=paste0('driver={SQL Server};server=', SRV, ';database=', DB,';username=', UN, ';password=', PW)

# turtle-tracks.Rmd
APITOKEN="Token paste-wastd-api-token"
