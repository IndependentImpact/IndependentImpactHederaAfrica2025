# Independent Impact Project Development Portal

## Project Title & Track
- **Title:** Independent Impact Project Development Portal
- **Hackathon Track:** 2. DLT for Operations
- **Pitch Deck:** _Replace this placeholder with the public link to your latest deck before submission._
- **Certification Link:** [Christiaan Pauw](https://bafkreiafrgokcpsbjlai5sxkmundbakelz74qgxcu6jj2u27klgrbl3g3u.ipfs.w3s.link/)

## Repository Overview
### Project Development Portal (primary focus)
The `ProjectDevelopmentPortal/` directory hosts the Independent Impact Project Development Portal. The portal is also hosted at https://app.independentimpact.org, currently running on testnet. Test it out by login in as userabcde12345@nomail.com with password *******. The Project Development Portal provides the infrastructure for the design, validation and verification for impact projects. Impact projects are any undertaking that is specifically intended to lead to a beneficial impact over and above what would have transpired in the normal course of events. This typically includes greenhouse gas emission reduction projects, but also climate change adaptation, health, and poverty alleviation. 

The app contains utilities for user registration and three different project-related workflows. 

The app registers agents, orchestrates workflow modules, and logs every action against Hedera Consensus Service and Fluree backends. The app sources `global.R`, builds the interface with `shiny`/`shinyjs`, and renders either the login module or the full portal depending on the active session state.【F:ProjectDevelopmentPortal/app.R†L2-L119】 The environment template wires key paths, Postgres settings, Fluree defaults, and the Hiero Hedera SDK bindings used throughout the portal.【F:ProjectDevelopmentPortal/template_global.R†L2-L85】 Bootstrapping scripts provision database tables, ingest workflow metadata, and prepare Hedera operator credentials for downstream transactions.【F:ProjectDevelopmentPortal/scripts/setup.R†L1-L160】

Hedera-specific operations cover creating project topics, registering DID-backed user identities, and submitting notarised updates. Project onboarding signs a `TopicCreateTransaction`, links admin/submit keys, and stores the resulting topic against the project record.【F:ProjectDevelopmentPortal/modules/tabProjects.R†L309-L360】 Agent enrolment generates Hedera accounts, registers Hiero DIDs, and pushes DID documents to IPFS, ensuring each contributor has a verifiable identifier anchored on Hedera Testnet.【F:ProjectDevelopmentPortal/modules/createIndImpAccount.R†L336-L438】 Document publication encrypts workflow payloads, decrypts topic keys, and executes `TopicMessageSubmitTransaction` calls before retrieving the mirror-sequenced message ID for audit trails.【F:ProjectDevelopmentPortal/functions/submitDocToHedera.R†L300-L360】

### GHG methodologies
`GHG_methodologies/` packages the top UNFCCC CDM methodologies as installable R libraries, with curated references, pkgdown-ready vignettes, and coverage across renewable energy, efficiency, and waste management methodologies for both small- and large-scale projects.【F:GHG_methodologies/README.md†L1-L135】 These packages inform the quantification logic surfaced inside the portal workflows.
Documentation is hosted on https://independentimpact.github.io/GHG_methodologies/. 

### Data Explorer
`IIchatbot/` provides a Docker Compose stack combining a Fluree MCP server, a Streamlit analyst UI, and an Nginx front end for Model Context Protocol interactions with ledger data.【F:IIchatbot/README.md†L1-L68】【F:IIchatbot/docker-compose.yml†L1-L50】 It supports analysts who need conversational access to the same datasets powering the portal.

The explorer is live at chat.independentimpact.org. 

### Protocol
`Protocol/` contains the Independent Impact governance blueprints—introduction, principles, scoring systems, voting, anti-gaming controls, and the Hedera-anchored technical implementation notes—which the portal enforces programmatically.【F:Protocol/00-Protocol.md†L1-L16】【F:Protocol/11-a-TechnicalImplementation.md†L1-L17】

## Hedera Integration Summary
### Hedera Consensus Service (HCS)
We use HCS topics for immutable project and workflow logging: every new project mints a dedicated topic whose admin and submit keys are stored locally, and every workflow submission posts encrypted IPFS payload references via `TopicMessageSubmitTransaction`. This design guarantees tamper-evident governance records while keeping the heavy documents off-chain.【F:ProjectDevelopmentPortal/modules/tabProjects.R†L309-L360】【F:ProjectDevelopmentPortal/functions/submitDocToHedera.R†L314-L360】 Predictable topic fees mean registries can budget for thousands of submissions without risking operational overruns—critical for African community projects operating on tight margins.【F:Protocol/11-a-TechnicalImplementation.md†L5-L17】

### Hedera Accounts & DID Registry
Agent onboarding mints Hedera accounts with `AccountCreateTransaction`, registers Hiero-backed DIDs, and stores the resulting identifiers against the user’s ledger profile. DID documents are encrypted, uploaded to IPFS, and linked to Hedera topics, giving reviewers verifiable digital identities without exposing private keys.【F:ProjectDevelopmentPortal/modules/createIndImpAccount.R†L336-L438】 Finality under three seconds keeps onboarding responsive even in regions with constrained connectivity, while DID anchoring eliminates manual credential vetting costs.【F:Protocol/11-a-TechnicalImplementation.md†L5-L17】

### Hedera Token Service (HTS)
The bootstrap script outlines a suite of non-fungible licenses and certificates—each created with `TokenCreateTransaction` and tied to specific workflows—for project developer, validator, and verifier roles.【F:ProjectDevelopmentPortal/scripts/setup.R†L200-L346】 Even when issuance is paused, the defined flow makes it trivial to turn on paid certifications, leveraging Hedera’s low minting costs to keep compliance affordable for African partners.【F:Protocol/11-a-TechnicalImplementation.md†L9-L17】

### Transaction Types
- `TopicCreateTransaction` for allocating per-project governance topics.【F:ProjectDevelopmentPortal/modules/tabProjects.R†L317-L330】
- `TopicMessageSubmitTransaction` for notarising workflow events and IPFS references.【F:ProjectDevelopmentPortal/functions/submitDocToHedera.R†L314-L323】
- `AccountCreateTransaction` for onboarding agents and linking wallets to portal identities.【F:ProjectDevelopmentPortal/modules/createIndImpAccount.R†L336-L358】
- `TokenCreateTransaction` for licensing and credential NFTs issued by standards workflows.【F:ProjectDevelopmentPortal/scripts/setup.R†L264-L281】

### Economic Justification
Every recorded action—project creation, reviewer commentary, or certificate issuance—maps to deterministic Hedera micro-fees noted in the protocol documentation.【F:Protocol/11-a-TechnicalImplementation.md†L5-L17】 Combined with ABFT finality and 10k+ TPS capacity, these stable costs let Independent Impact subsidise onboarding for smaller African developers today and scale to national programmes without renegotiating treasury budgets.

## Deployment & Setup Instructions
1. **Clone the repository**
   ```bash
   git clone https://github.com/<your-org>/IndependentImpactHederaAfrica2025.git
   cd IndependentImpactHederaAfrica2025
   ```
2. **Install R dependencies**: Ensure R 4.3+ is available, then install the packages referenced in `template_global.R` (`shiny`, `shinyjs`, `shinythemes`, `tidyr`, `tibble`, `httr`, `RJDBC`, `RPostgreSQL`, `PKI`, `plotly`, `hedera`, `jellyfi3shR`, `logger`, `sf`, `leaflet`, `novaRush`, `reticulate`, `plyr`).【F:ProjectDevelopmentPortal/template_global.R†L16-L43】
3. **Prepare Python environment**: Create a virtual environment and install the Hiero SDK dependencies listed in `scripts/setup.R` (`protobuf>=5.27,<5.29`, `hiero-did-sdk-python`, `hiero-sdk-python`, `requests`, `python-dotenv`). Record the interpreter path for later use.【F:ProjectDevelopmentPortal/scripts/setup.R†L3-L8】
4. **Configure `global.R`**: Copy `ProjectDevelopmentPortal/template_global.R` to `ProjectDevelopmentPortal/global.R`, replace every `REPLACE_ME` placeholder with your local paths, Postgres host, database, and port, and confirm `hederaNetwork` stays on `testnet`. Set `reticulate::use_python` to the interpreter from the previous step.【F:ProjectDevelopmentPortal/template_global.R†L2-L85】
5. **Provide secrets via `iwefdj`**: Populate the `tmp/uiguhk.Rda` file (or equivalent secure loader) with an `iwefdj` list that stores Hedera operator credentials, Cyphr key paths/passwords, Postgres username/password, Python path, Web3.storage configuration, and service emails referenced across the modules.【F:ProjectDevelopmentPortal/template_global.R†L54-L85】【F:ProjectDevelopmentPortal/functions/dbAuxFunctions/getDbCon.R†L22-L27】【F:ProjectDevelopmentPortal/modules/createIndImpAccount.R†L343-L418】
6. **Set environment variables**: Export `API_KEY_FLUREE` (for the Fluree ledger) and any additional secrets consumed by Docker services or helper scripts.【F:ProjectDevelopmentPortal/scripts/setup.R†L6-L13】
7. **Provision the database**: Run the one-time bootstrap script to create tables, load workflows, schemas, and (optionally) token definitions. Use a dedicated Hedera operator account with sufficient Testnet HBAR when running this step.
   ```bash
   Rscript ProjectDevelopmentPortal/scripts/setup.R
   ```
   The script iterates over SQL files in `table-definitions/`, registers workflow metadata, and links operator keys to the ledger.【F:ProjectDevelopmentPortal/scripts/setup.R†L18-L160】【F:ProjectDevelopmentPortal/scripts/setup.R†L200-L346】
8. **Launch the portal**: Start the Shiny app in Testnet mode.
   ```bash
   R -e "shiny::runApp('ProjectDevelopmentPortal')"
   ```
   The UI listens on `http://127.0.0.1:8100` by default (set via `options(shiny.port)` if needed) and alternates between login and project dashboards using the modules defined in `app.R`. Expect to run the portal alongside Postgres, the Hiero SDK Python runtime, and the Fluree ledger connection.【F:ProjectDevelopmentPortal/app.R†L4-L119】

### Running Environment
- **Frontend:** R Shiny (`shiny::runApp`) serving the Project Development Portal UI on `localhost`.
- **Backend services:** R scripts leverage Postgres via `RPostgreSQL`, Fluree via `novaRush`, IPFS uploads via the Web3 CLI, and Hedera via the Hiero Python SDK bridged through `reticulate`.
- **Optional analytics:** Bring up `IIchatbot` with `docker compose up` inside `IIchatbot/` to expose the Streamlit MCP dashboard for data exploration.【F:IIchatbot/README.md†L33-L60】

## Architecture Diagram
```
+----------------------+       +----------------------+       +-------------------------------+
|  R Shiny Frontend    | <---> |  R Services & DB     | <---> | Hedera SDK (Hiero via Python) |
|  (modules/, app.R)   |       |  (Postgres, Fluree)  |       |  + Hedera Testnet & Mirror    |
+----------+-----------+       +----------+-----------+       +---------------+---------------+
           |                              |                                    |
           |                              |                                    v
           |                              |                        Topic/Tx records & DID state
           v                              v                                    |
     Web3/IPFS CLI                 Workflow scripts                        Mirror queries
```
Data flows from the Shiny UI into R modules that persist state in Postgres/Fluree, sign Hedera transactions through the Hiero SDK, and pin documents to IPFS before recording immutable references on Hedera.

## Deployed Hedera IDs (Testnet)
- **Methodology Schema Topic:** `0.0.5622522` (`messageId 1740491068.879838270`) owned by DID `did:hedera:testnet:..._0.0.5622502`; document stored at `ipfs://bafybeidtmwpb5osb6akmhzye63ordlqvy7qdla22njcaxetoqzc4gmbola`.【F:ProjectDevelopmentPortal/schemas/#7f477eea-d0ab-4ad7-a448-f771c58f22bb&1.0.0.json†L1】
- **Impact Schema Topic:** `0.0.5622522` (`messageId 1740658973.415082000`) with the same DID controller and IPFS CID `bafybeighww5iocg2orqc6ciidj7ajdyadnvlp66a355duebn5ignczxgdy`.【F:ProjectDevelopmentPortal/schemas/#7283faa7-578d-471d-aa7f-04ff62b795e1&3.0.0.json†L1】
- **Operator DID Account:** `0.0.5622502`, referenced as creator/owner for schema publications and resolved through Hiero DID registration during onboarding.【F:ProjectDevelopmentPortal/schemas/#7f477eea-d0ab-4ad7-a448-f771c58f22bb&1.0.0.json†L1】【F:ProjectDevelopmentPortal/modules/createIndImpAccount.R†L336-L418】

Update this section as you deploy additional workflows (e.g., license tokens, review topics, mirror node queries) so judges can replay transactions during evaluation.

