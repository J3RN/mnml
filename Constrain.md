Steps:

1. Extract type definitions (Define each constructor as a value)
   - Each type definition becomes 1 type and N values (its constructors)
   - The return type of each constructor is the type being defined; their arguments may be types not yet known (create temporary types to be resolved later)
2. Extract type aliases
   - Each type alias becomes 1 type
3. Reconcile temporary types
   - **Is this the right place to do this?**
   - For each temporary type that was defined, we find its "real" type.
     - If we find its "real" type, replace all instances of the temporary type with the "real" type.
     - If we _cannot_ find its "real" type, mark this is an error.
4. Extract value definitions
   - Each value definition becomes 1 value
   - All reference types are represented by a "temporary value type"
