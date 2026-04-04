#ifndef PUMA_FILE_DIRECTORY_HPP
#define PUMA_FILE_DIRECTORY_HPP

#pragma once

#include "../PumaType/String.hpp"

namespace Puma {
namespace File
{
    namespace Directory
    {
        // Get current working directory - normalized to forward slashes
        Type::String GetCurrentDirectory() noexcept;

        // Set current working directory - accepts a Puma String path
        bool SetCurrentDirectory(const Type::String& path) noexcept;
    } // namespace Directory
} // namespace File
} // namespace Puma

#endif // PUMA_FILE_DIRECTORY_HPP