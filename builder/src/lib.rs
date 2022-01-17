use proc_macro::TokenStream;
use quote::{format_ident, quote, ToTokens};
use syn::{
    parse_macro_input, punctuated::Punctuated, token::Comma, AngleBracketedGenericArguments,
    Attribute, Data, DataStruct, DeriveInput, Field, Fields, FieldsNamed, GenericArgument, Ident,
    Lit, Meta, MetaNameValue, NestedMeta, Path, PathArguments, PathSegment, Type, TypePath,
};

#[proc_macro_derive(Builder, attributes(builder))]
pub fn derive(input: TokenStream) -> TokenStream {
    let input = parse_macro_input!(input as DeriveInput);

    if let Data::Struct(DataStruct {
        fields: Fields::Named(FieldsNamed { named, .. }),
        ..
    }) = &input.data
    {
        let origin_impl = origin_impl(&input, named);
        let builder_struct = builder_struct(&input, named);
        let builder_impl = builder_impl(&input, named);

        quote! {
            #origin_impl
            #builder_struct
            #builder_impl
        }
        .into()
    } else {
        unimplemented!()
    }
}

fn origin_impl(input: &DeriveInput, named: &Punctuated<Field, Comma>) -> impl ToTokens {
    let origin_struct_ident = &input.ident;
    let builder_struct_ident = format_ident!("{}Builder", origin_struct_ident);

    let builder_struct_fields_default = named.iter().map(|field| {
        let field_ident = &field.ident;
        let field_ty = &field.ty;
        if ty_check_and_get_inner_ty(field_ty, "Vec").is_some() {
            quote!(#field_ident: Some(vec!()))
        } else {
            quote!(#field_ident: None)
        }
    });
    quote! {
        impl #origin_struct_ident {
            pub fn builder() -> #builder_struct_ident {
                #builder_struct_ident {
                    #(#builder_struct_fields_default),*
                }
            }
        }
    }
}

fn builder_struct(input: &DeriveInput, named: &Punctuated<Field, Comma>) -> impl ToTokens {
    let origin_struct_ident = &input.ident;
    let builder_struct_ident = format_ident!("{}Builder", origin_struct_ident);

    let builder_struct_fields = named.iter().map(|field| {
        let field_ident = &field.ident;
        let field_ty = if let Some(inner_ty) = ty_check_and_get_inner_ty(&field.ty, "Option") {
            inner_ty
        } else {
            &field.ty
        };
        quote!(#field_ident: Option<#field_ty>)
    });
    quote! {
        pub struct #builder_struct_ident {
            #(#builder_struct_fields),*
        }
    }
}

fn builder_impl(input: &DeriveInput, named: &Punctuated<Field, Comma>) -> impl ToTokens {
    let origin_struct_ident = &input.ident;
    let builder_struct_ident = format_ident!("{}Builder", origin_struct_ident);

    let builder_fields = named.iter().map(|field| {
        let field_ident = &field.ident;
        let field_ty = &field.ty;
        if ty_check_and_get_inner_ty(field_ty, "Option").is_some() {
            quote!(#field_ident: self.#field_ident.clone())
        } else {
            quote!(#field_ident: self.#field_ident.clone().ok_or(concat!(stringify!(#field_ident), " has not been set yet"))?)
        }
    });
    let builder_setters = named.iter().map(|field| {
        let field_attrs = &field.attrs;
        let field_ident = &field.ident;
        let field_ty = if let Some(inner_ty) = ty_check_and_get_inner_ty(&field.ty, "Option") {
            inner_ty
        } else {
            &field.ty
        };
        let setter = quote! {
            pub fn #field_ident(&mut self, #field_ident: #field_ty) -> &mut Self {
                self.#field_ident = Some(#field_ident);
                self
            }
        };

        if field_attrs.is_empty() {
            quote!(#setter)
        } else {
            // #[builder(each = "...")]
            let vec_inner_ty =
                ty_check_and_get_inner_ty(field_ty, "Vec").expect("expect type is Vec");
            let each_ident = get_each_ident(field_attrs);
            quote! {
                fn #each_ident(&mut self, #each_ident: #vec_inner_ty) -> &mut Self {
                    if let Some(ref mut v) = self.#field_ident {
                        v.push(#each_ident);
                    } else {
                        self.#field_ident = Some(vec![#each_ident]);
                    }
                    self
                }
            }
        }
    });
    quote! {
        impl #builder_struct_ident {
            pub fn build(&mut self) -> Result<#origin_struct_ident, Box<dyn std::error::Error>> {
                Ok(
                    #origin_struct_ident {
                        #(#builder_fields),*
                    }
                )
            }
            #(#builder_setters)*
        }
    }
}

fn ty_check_and_get_inner_ty<'a>(ty: &'a Type, expected_ident: &str) -> Option<&'a Type> {
    if let Type::Path(TypePath {
        path: Path { segments, .. },
        ..
    }) = ty
    {
        if segments.len() != 1 {
            return None;
        }
        if let PathSegment {
            ident,
            arguments: PathArguments::AngleBracketed(AngleBracketedGenericArguments { args, .. }),
        } = &segments[0]
        {
            if ident == expected_ident {
                if let Some(GenericArgument::Type(inner_ty)) = args.first() {
                    return Some(inner_ty);
                }
            }
        }
    }
    None
}

fn get_each_ident(attrs: &Vec<Attribute>) -> Ident {
    if attrs.len() != 1 {
        unimplemented!();
    }
    if let Ok(Meta::List(list)) = attrs[0].parse_meta() {
        if list.nested.len() != 1 {
            unimplemented!();
        }

        if list.path.is_ident("builder") {
            if let NestedMeta::Meta(Meta::NameValue(MetaNameValue {
                path,
                lit: Lit::Str(lit_str),
                ..
            })) = &list.nested[0]
            {
                if path.is_ident("each") {
                    if let Ok(ident) = lit_str.parse() {
                        return ident;
                    }
                }
            }
        }
    }
    unimplemented!()
}
