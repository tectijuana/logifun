%------------------------------------------------------------------------------
% Módulo:       hola_mundo
% Propósito:    Imprimir un mensaje de "Hola, Mundo!" en la consola.
% 
% Autor:        René Solís
% Fecha:        22 de Agosto de 2024
% Versión:      1.0
%
% Descripción: 
% Este módulo define una función simple que imprime "Hola, Mundo!" en la consola.
% Es un ejemplo introductorio para mostrar la estructura básica de un programa
% en Erlang, así como el uso de funciones de entrada/salida.
%
% Dependencias:
% Ninguna.
%
% Ejemplo de Uso:
% 1. Compilar el módulo: 
%    c(hola_mundo).
% 2. Llamar a la función:
%    hola_mundo:saludar().
%
% Historial de Cambios:
% 22/08/2024 - 1.0 - Creación del módulo con función básica `saludar/0`.
%
% Notas:
% Este es un ejemplo básico y no cubre aspectos avanzados de Erlang como 
% concurrencia o manejo de errores.
%------------------------------------------------------------------------------

% Esta es la definición del módulo para el programa Erlang.
% El nombre del módulo debe coincidir con el nombre del archivo (sin la extensión .erl).
-module(hola_mundo).

% Esta línea exporta la función `saludar/0` para hacerla accesible desde fuera del módulo.
% El `/0` indica que la función no recibe argumentos.
-export([saludar/0]).

% Definición de la función
% `saludar/0` es una función que imprime "Hola, Mundo!" en la consola.
saludar() ->
    % `io:format/1` es una función incorporada que se utiliza para la salida de texto.
    % Toma una cadena de formato y la imprime en la salida estándar.
    % "~n" es un especificador de formato que representa un carácter de nueva línea.
    io:format("Hola, Mundo!~n").

