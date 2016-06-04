#include "window-private.h"

#include "shader.h"

static const GLchar* vertexsource = "\
#version 150\n\
in vec2 position;\n\
out vec2 textureCoord;\n\
uniform mat3 uMvp;\n\
void main(void) {\n\
    textureCoord = position;\n\
    gl_Position = vec4(uMvp * vec3(position, 1.0), 1.0);\n\
}\n";

static const GLchar* fragmentsource = "\
#version 150\n\
precision highp float;\n\
uniform bool uMode;\n\
uniform vec4 uColor;\n\
uniform sampler2D uTexture;\n\
in vec2 textureCoord;\n\
out vec4 outColor;\n\
void main(void) {\n\
    if (uMode) {\n\
        outColor = texture2D(uTexture, textureCoord);\n\
    } else {\n\
        outColor = uColor;\n\
    }\n\
}\n";

static int getCompileStatus(int isProgram, GLuint object) {
    GLint success;
    if (isProgram) {
        glGetProgramiv(object, GL_LINK_STATUS, &success);
    } else {
        glGetShaderiv(object, GL_COMPILE_STATUS, &success);
    }

    if (success == GL_FALSE) {
        int maxLength;

        if (isProgram) {
            glGetShaderiv(object, GL_INFO_LOG_LENGTH, &maxLength);
        } else {
            glGetShaderiv(object, GL_INFO_LOG_LENGTH, &maxLength);
        }

        GLchar* infoLog = (GLchar*) malloc(maxLength);

        glGetShaderInfoLog(object, maxLength, &maxLength, infoLog);

        puts(infoLog);

        free(infoLog);
        return 0;
    } else {
        return 1;
    }
}

static int compileShaderObject(int isVertex, GLuint* outShader) {
    GLenum type = isVertex ?  GL_VERTEX_SHADER : GL_FRAGMENT_SHADER;
    GLuint shader = glCreateShader(type);

    const GLchar* source = isVertex ? vertexsource : fragmentsource;
    glShaderSource(shader, 1, &source, 0);

    glCompileShader(shader);

    int status = getCompileStatus(0, shader);

    if (status) {
        *outShader = shader;
        return 1;
    } else {
        return 0;
    }
}

static int compileProgram(GLuint* outProgram) {
    GLuint vertex;
    GLuint fragment;
    int vertexSuccess = compileShaderObject(1, &vertex);
    int fragmentSuccess = compileShaderObject(0, &fragment);

    if (!vertexSuccess || !fragmentSuccess) return 0;

    GLuint program = glCreateProgram();

    glAttachShader(program, vertex);
    glAttachShader(program, fragment);

    glLinkProgram(program);

    int status = getCompileStatus(1, program);

    if (status) {
        *outProgram = program;
        return 1;
    } else {
        return 0;
    }
}

int compileShader(Window* w) {
    GLuint shader;
    int success = compileProgram(&shader);
    if (!success) return 0;

    w->shaderMvp = glGetUniformLocation(shader, "uMvp");
    w->shaderMode = glGetUniformLocation(shader, "uMode");
    w->shaderColor = glGetUniformLocation(shader, "uColor");
    w->shaderTexture = glGetUniformLocation(shader, "uTexture");

    const GLfloat vertices[] = {
        0, 0, // Top-left
        1, 0, // Bottom-left
        0, 1, // Top-right
        1, 1  // Bottom-right
    };

    const GLubyte elements[] = {
        0, 1, 2,
        1, 3, 2
    };

    GLuint vao;
    glGenVertexArrays(1, &vao);

    GLuint vbo;
    glGenBuffers(1, &vbo);

    GLuint ebo;
    glGenBuffers(1, &ebo);

    glBindVertexArray(vao);

    glBindBuffer(GL_ARRAY_BUFFER, vbo);
    glBufferData(GL_ARRAY_BUFFER,
            sizeof(vertices), vertices, GL_STATIC_DRAW);

    glBindBuffer(GL_ELEMENT_ARRAY_BUFFER, ebo);
    glBufferData(GL_ELEMENT_ARRAY_BUFFER,
            sizeof(elements), elements, GL_STATIC_DRAW);

    GLint posAttrib = glGetAttribLocation(shader, "position");
    glVertexAttribPointer(posAttrib, 2, GL_FLOAT, GL_FALSE, 0, 0);
    glEnableVertexAttribArray(posAttrib);

    glUseProgram(shader);

    return 1;
}
