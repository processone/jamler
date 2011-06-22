#include <stdio.h>
#include <string.h>
#include <caml/mlvalues.h>
#include <caml/memory.h>
#include <caml/alloc.h>
#include <caml/callback.h>
#include <expat.h>

struct ml_parser 
{
      XML_Parser p;
      value start_element_handler;
      value end_element_handler;
      value character_data_handler;
};



value mlXML_ParserCreate (value encoding)
{
   XML_Parser p;
   struct ml_parser *mlp;
   
   p = XML_ParserCreate(String_val(encoding));

   mlp = malloc(sizeof(*mlp));
   if(mlp == NULL)
   {
      perror("mlXML_ParserCreate");
      /* TODO */
   }
   
   mlp->p = p;
   mlp->start_element_handler = 0;
   mlp->end_element_handler = 0;
   mlp->character_data_handler = 0;
   
   XML_SetUserData(p, mlp);
   
   //printf("<< XML_ParserCreate >>\n") ; fflush(stdout) ;
   return Val_long(p);
}

value mlXML_Parse(value p, value s, value isFinal)
{
   int len = string_length(s);
   char *str = String_val(s);
   XML_Parser parser = (XML_Parser)Long_val(p);
   void *buf = XML_GetBuffer(parser, len);

   memcpy(buf, str, len);

   return Val_bool(XML_ParseBuffer(parser, len, Bool_val(isFinal)));
}

static void mlXML_StartElementHandler(struct ml_parser *mlp,
				      const XML_Char *name,
				      const XML_Char **atts)
{
   CAMLparam0();
   CAMLlocal4(attr_list, tmp, tmp2, str);
   int i;

   for (i = 0; atts[i]; i += 2) {}

   attr_list = Val_emptylist;
   for (i -= 2; i >= 0; i -= 2)
   {
      tmp = alloc_tuple(2);
      tmp2 = alloc_tuple(2);
      Store_field(tmp, 0, tmp2);
      Store_field(tmp2, 0, copy_string(atts[i]));
      Store_field(tmp2, 1, copy_string(atts[i+1]));
      Store_field(tmp, 1, attr_list);
      attr_list = tmp;
   }

   str = copy_string(name);
   caml_callback2(mlp->start_element_handler, str, attr_list);
   CAMLreturn0;
}

value mlXML_SetStartElementHandler(value p, value handler)
{
   struct ml_parser *mlp;

   mlp = XML_GetUserData((XML_Parser)Long_val(p));
   caml_register_global_root(&mlp->start_element_handler);
   mlp->start_element_handler = handler;

   XML_SetStartElementHandler(
      (XML_Parser)Long_val(p),
      (XML_StartElementHandler) mlXML_StartElementHandler);
   return Val_unit;
}


static void mlXML_EndElementHandler(struct ml_parser *mlp,
				    const XML_Char *name)
{
   value str = copy_string(name);
   caml_callback(mlp->end_element_handler, str);
}

value mlXML_SetEndElementHandler(value p, value handler)
{
   struct ml_parser *mlp;

   mlp = XML_GetUserData((XML_Parser)Long_val(p));
   caml_register_global_root(&mlp->end_element_handler);
   mlp->end_element_handler = handler;

   XML_SetEndElementHandler(
      (XML_Parser)Long_val(p),
      (XML_EndElementHandler) mlXML_EndElementHandler);
   return Val_unit;
}

static void mlXML_CharacterDataHandler(struct ml_parser *mlp,
				       const XML_Char *s,
				       int len)
{
   value str = alloc_string(len);
   memcpy(String_val(str), s, len);
   caml_callback(mlp->character_data_handler, str);
}

value mlXML_SetCharacterDataHandler(value p, value handler)
{
   struct ml_parser *mlp;

   mlp = XML_GetUserData((XML_Parser)Long_val(p));
   caml_register_global_root(&mlp->character_data_handler);
   mlp->character_data_handler = handler;

   XML_SetCharacterDataHandler(
      (XML_Parser)Long_val(p),
      (XML_CharacterDataHandler) mlXML_CharacterDataHandler);
   return Val_unit;
}

value mlXML_GetErrorCode(value p)
{
   return Val_int(XML_GetErrorCode((XML_Parser)Long_val(p)));
}

value mlXML_ErrorString(value code)
{
   return copy_string(XML_ErrorString(Int_val(code)));
}

value mlXML_ParserFree(value p)
{
   XML_Parser parser = (XML_Parser)Long_val(p);
   struct ml_parser *mlp = XML_GetUserData(parser);

   XML_ParserFree(parser);
   if (mlp->start_element_handler != 0)
      caml_remove_global_root(&mlp->start_element_handler);
   if (mlp->end_element_handler != 0)
      caml_remove_global_root(&mlp->end_element_handler);
   if (mlp->character_data_handler != 0)
      caml_remove_global_root(&mlp->character_data_handler);

   free(mlp);

   return Val_unit;
}

