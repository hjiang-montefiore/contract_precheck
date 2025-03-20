# Contract Precheck

This tool provides a self-service contract precheck to examine whether the uploaded file contains any of the following issues:

    1. Invalid Column Names
        - The column names must follow the predefined format. Any deviations will be flagged.

    2. Duplications
        - Checks if the contract contains multiple entries with the same manufacturer catalog number.

    3. UOM (Unit of Measure) Violations
        - The following UOM-related issues will be detected:
          i) Unknown UOM – The UOM is not recognized.
          ii) Non-Recommended UOM – The UOM is known but should be replaced with the recommended one.
          iii) EA UOM with Incorrect QOE – If the UOM is "EA" (Each), the QOE (Quantity per Order) must be 1.

This precheck ensures contract data integrity before proceeding with further processing. 🚀
