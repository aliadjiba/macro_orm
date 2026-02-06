// #[proc_macro_attribute]
// pub fn belongs_to_many(attr: TokenStream, item: TokenStream) -> TokenStream {
//     let input = parse_macro_input!(item as syn::ItemStruct);
//     let struct_name = &input.ident;

//     // Parse comma-separated list of pivot structs
//     let pivots: syn::punctuated::Punctuated<syn::Path, syn::token::Comma> =
//         parse_macro_input!(attr with syn::punctuated::Punctuated::parse_terminated);

//     let mut methods = Vec::new();

//     for pivot in pivots {
//         // Generate a type ident for the pivot
//         let pivot_ident = pivot.segments.last().unwrap().ident.clone();

//         // Generate a method name (we'll fix later to the other side)
//         let method_name = syn::Ident::new(
//             &pivot_ident.to_string().to_lowercase().replace("pivot", ""),
//             struct_name.span(),
//         );

//         // Generate method code
//         let method = quote! {
//             pub fn #method_name<'a>(&self, repo: &'a orm::repository::Repo) -> orm::model::BelongsToMany<'a, #pivot_ident> {
//                 use orm::model::Pivot;

//                 // Determine which side of the pivot matches this struct
//                 let is_left = {
//                     let left_model = #pivot_ident::left_model();
//                     left_model == Self::table_name()
//                 };

//                 orm::model::BelongsToMany::new(repo, self.id.clone(), is_left)
//             }
//         };

//         methods.push(method);
//     }

//     let expanded = quote! {
//         #input

//         impl #struct_name {
//             #(#methods)*
//         }
//     };

//     TokenStream::from(expanded)
// }


// #[proc_macro_derive(PivotModel, attributes(left, right))]
// pub fn pivot_model_derive(input: TokenStream) -> TokenStream {
//     let input = parse_macro_input!(input as DeriveInput);
//     let struct_name = input.ident;
//     let data = match input.data {
//         Data::Struct(data) => data,
//         _ => panic!("PivotModel can only be derived for structs"),
//     };
//     let fields = match data.fields {
//         Fields::Named(fields) => fields.named,
//         _ => panic!("PivotModel requires named fields"),
//     };
//     let mut left_field = None;
//     let mut right_field = None;
//     for field in fields.iter() {
//         for attr in &field.attrs {
//             if attr.path().is_ident("left") {
//                 left_field = Some(field.ident.clone().unwrap());
//             }
//             if attr.path().is_ident("right") {
//                 right_field = Some(field.ident.clone().unwrap());
//             }
//         }
//     }
//     let left_ident = left_field.expect("Missing #[left] field");
//     let right_ident = right_field.expect("Missing #[right] field");
//     let left_key = left_ident.to_string();
//     let right_key = right_ident.to_string();
//     // auto snake_case table name
//     let table_name = struct_name.to_string()
//         .replace("Pivot", "")
//         .to_lowercase();
//     let expanded = quote! {
//         impl Model for #struct_name {
//             fn table_name() -> &'static str {
//                 #table_name
//             }
//             fn id(&self) -> surrealdb::sql::Thing {
//                 self.id.clone()
//             }
//         }
//         impl Pivot for #struct_name {
//             fn left_key() -> &'static str {
//                 #left_key
//             }
//             fn right_key() -> &'static str {
//                 #right_key
//             }
//             fn left_id(&self) -> &surrealdb::sql::Thing {
//                 &self.#left_ident
//             }
//             fn right_id(&self) -> &surrealdb::sql::Thing {
//                 &self.#right_ident
//             }
//             fn new(left: surrealdb::sql::Thing, right: surrealdb::sql::Thing) -> Self {
//                 let now = chrono::Utc::now().to_rfc3339();
//                 Self {
//                     id: surrealdb::sql::Thing::from((
//                         Self::table_name().to_string(),
//                         surrealdb::Uuid::new_v4().to_string()
//                     )),
//                     #left_ident: left,
//                     #right_ident: right,
//                     created_at: now.clone(),
//                     updated_at: now,
//                 }
//             }
//         }
//     };
//     TokenStream::from(expanded)
// }



// #[proc_macro_derive(PivotModel, attributes(pivot, left, right))]
// pub fn pivot_model(input: TokenStream) -> TokenStream {
//     let input = parse_macro_input!(input as DeriveInput);
//     let struct_name = &input.ident;

//     // 1️⃣ Find #[pivot(Left, Right)]
//     let pivot_attr: Option<&Attribute> = input.attrs.iter().find(|a| a.path().is_ident("pivot"));
//     let pivot_attr = match pivot_attr {
//         Some(p) => p,
//         None => {
//             return syn::Error::new_spanned(
//                 &input,
//                 "PivotModel requires #[pivot(LeftType, RightType)]",
//             )
//             .to_compile_error()
//             .into()
//         }
//     };

//     // 2️⃣ Parse types inside #[pivot(...)]
//     let parser = Punctuated::<Path, Comma>::parse_terminated;
//     let paths = match pivot_attr.parse_args_with(parser) {
//         Ok(p) => p,
//         Err(e) => return e.to_compile_error().into(),
//     };

//     if paths.len() != 2 {
//         return syn::Error::new_spanned(
//             &pivot_attr,
//             "Expected exactly two types in #[pivot(LeftType, RightType)]",
//         )
//         .to_compile_error()
//         .into();
//     }

//     let left_type = paths[0].clone();
//     let right_type = paths[1].clone();

//     // 3️⃣ Detect #[left] and #[right] fields
//     let mut left_field: Option<Ident> = None;
//     let mut right_field: Option<Ident> = None;

//     if let Data::Struct(data_struct) = &input.data {
//         if let Fields::Named(fields_named) = &data_struct.fields {
//             for field in &fields_named.named {
//                 for attr in &field.attrs {
//                     if attr.path().is_ident("left") {
//                         left_field = field.ident.clone();
//                     }
//                     if attr.path().is_ident("right") {
//                         right_field = field.ident.clone();
//                     }
//                 }
//             }
//         }
//     }

//     if left_field.is_none() || right_field.is_none() {
//         return syn::Error::new_spanned(
//             &input,
//             "PivotModel requires #[left] and #[right] fields",
//         )
//         .to_compile_error()
//         .into();
//     }

//     let left_field = left_field.unwrap();
//     let right_field = right_field.unwrap();

//     // 4️⃣ Generate Pivot implementation
//     let expanded = quote! {
//         impl orm::model::Pivot for #struct_name {
//             fn left_key() -> &'static str { stringify!(#left_field) }
//             fn right_key() -> &'static str { stringify!(#right_field) }

//             fn left_id(&self) -> &surrealdb::sql::Thing { &self.#left_field }
//             fn right_id(&self) -> &surrealdb::sql::Thing { &self.#right_field }

//             fn new(left: surrealdb::sql::Thing, right: surrealdb::sql::Thing) -> Self {
//                 let now = chrono::Utc::now().to_rfc3339();
//                 Self {
//                     id: surrealdb::sql::Thing::from((Self::table_name().to_string(), surrealdb::Uuid::new_v4().to_string())),
//                     #left_field: left,
//                     #right_field: right,
//                     created_at: now.clone(),
//                     updated_at: now,
//                     ..Default::default()
//                 }
//             }
//         }
//         impl orm::model::Model for #struct_name {
//             fn table_name() -> &'static str {
//                 stringify!(#struct_name).to_lowercase().as_str() // or use a `#[table("name")]` attribute
//             }

//             fn id(&self) -> surrealdb::sql::Thing {
//                 self.id.clone()
//             }
//         }
//         impl Default for #struct_name {
//     fn default() -> Self {
//         let now = chrono::Utc::now().to_rfc3339();

//         Self {
//             id: Thing::from((stringify!(#struct_name).to_lowercase(), Uuid::new_v4().to_string())),
//             #left_field: Thing::from((stringify!(#left_field).to_lowercase(), Uuid::new_v4().to_string())),
//             #right_field: Thing::from((stringify!(#right_field).to_lowercase(), Uuid::new_v4().to_string())),
//             created_at: now.clone(),
//             updated_at: now,
//         }
//     }
// }


//     };

//     TokenStream::from(expanded)
// }